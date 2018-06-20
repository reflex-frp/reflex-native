{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | A generic builder for implementing 'Reflex.Adjustable' and 'Reflex.NotReady' on top of mutable view hierarchies efficiently.
--
-- This module contains the main function 'adjustingBuilder' along with specializations of that function for various data structures, as required for
-- implementing 'Reflex.Adjustable'.
--
-- @adjustingBuilder@ is quite complicated because of the intersection of mutability, performance requirements, and @NotReady@; these complications are
-- described in more detail at 'adjustingBuilder'.
--
-- The underlying mutable hierarchy it manipulates can be configured by the 'AdjustingBuilderConfig' you pass it. Each of the UI platforms (UIKit, DOM, etc)
-- have their own config which adapts @adjustingBuilder@ to the particular types and operations. In addition, 'Reflex.Native.Test.Builder' implements a
-- straightforward version which can be used to unit test both @adjustingBuilder@ itself and any UI components which are polymorphic in the view builder monad
-- and only require 'Reflex.Native.ViewBuilder.Class.ViewBuilder'.
module Reflex.Native.AdjustingBuilder
  (
  -- * Core builder
    adjustingBuilder
  , AdjustingBuilderConfig(..)
  -- * Specializations matching 'Reflex.Adjustable' methods
  , runWithReplaceImpl
  , traverseIntMapWithKeyWithAdjustImpl
  , traverseDMapWithKeyWithAdjustImpl
  , traverseDMapWithKeyWithAdjustWithMoveImpl
  ) where

import Control.Applicative ((<$>), pure)
import Control.Monad (Monad, (>>=), unless, when)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (bimap)
import Data.Bool (Bool(False, True))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum((:=>)))
import Data.Either (Either(Left, Right), either)
import Data.Foldable (foldlM, for_)
import Data.Functor (Functor(fmap), ($>))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Misc (ComposeMaybe(..))
import Data.GADT.Compare (GCompare)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.Some (Some, withSome)
import qualified Data.Some as Some
import Data.Word (Word)
import Reflex
  ( Adjustable, runWithReplace, traverseIntMapWithKeyWithAdjust, traverseDMapWithKeyWithAdjust, traverseDMapWithKeyWithAdjustWithMove
  , Event, MonadHold, numberOccurrencesFrom
  )
import Reflex.NotReady.Class (NotReady(notReady))
import Reflex.Patch.DMap (PatchDMap(..), mapPatchDMap)
import Reflex.Patch.DMapWithMove (From(..), NodeInfo(..), PatchDMapWithMove(..), mapPatchDMapWithMove, unPatchDMapWithMove)
import Reflex.Patch.IntMap (PatchIntMap(..))
import Reflex.Requester.Class (Requester(type Request), requesting_)


-- |@adjustingBuilder@ implements the various methods of 'Reflex.Adjustable' against some underlying mutable view hierarchy, e.g. DOM, UIKit, etc.; it installs
-- zero or many view fragments into the parent view based on a traversal function @k a -> v a -> m (v' a)@ applied to some initial structure and inserts new,
-- replaces old, moves around, or deletes those fragments in response to incoming patches to the structure.
--
-- This is made complicated by several things:
--
--    1. Each fragment can be unready (that is, use 'notReady' or 'notReadyUntil'), or be ready. If a fragment is unready and in the first generation (the
--    initial structure) then it should be installed in the parent view but the parent view marked as unready, and later marked as ready when each child
--    fragment has been ready once. If a fragment is unready but not in the first generation, then it should not be installed until it becomes ready.
--    2. Becoming ready is asynchronous with respect to new generations, and so an older generation could become ready after a newer generation has replaced
--    the fragment, and the older generation should not be installed in that case.
--    3. If the parent was marked unready because one of the initial generation was unready, it should be marked ready as soon as there's a fully ready set of
--    fragments. That is if the structure included keys k1 and k2 and k1 was ready but k2 was not in the initial generation, either k2 becoming ready in the
--    first generation or later being replaced by a later generation that was immediately or became ready should cause the parent to become ready. Later
--    generations after this initial parent ready don't get installed until they become ready, leaving the previous generation in place until then.
--    4. Being unready/ready is (usually) counted as an @IORef Word@ , and so it's important to not accidentally over-ready the parent.
--    5. The structure being used can vary widely in "strength", from 'Identity' up to 'DMap'. This has to work with any of those structures, but also needs
--    to keep additional data keyed the same way but without dependently typed data.
--    6. The patches being applied to the structure can vary widely from simple replacement up to 'PatchDMapWithMove', and so the associated actions to perform
--    can also vary. In the simple case, the previous fragment is replaced. In the complicated case, a fragment can be moved from one slot to another, cause
--    a slot to be removed entirely, replace the contents of a slot, or add new slots.
--    7. Views are semantically associated with a set of additional objects that have to be retained for things to work (e.g. target/action target objects), and
--    those objects must be intentionally forgotten whenever a new generation commits.
--    8. View fragments can be moved from key to key by patches which support move operations, and those view fragments could not be ready yet. When they
--    become ready they should install to the new location not the old one.
--
-- Internally, the keys of the structure over time are reflected by zero or more "slots" which retain non-view objects, keep track of which generation has most
-- recently been installed, and refer to an marker view which locates the view fragment in the subviews list of the parent. As each generation gets applied or
-- view fragments become ready the slots are consulted and updated to address the various complications above.
--
-- @adjustingBuilder@ is parameterized over a bunch of things:
--
--    * @key :: * -> *@, the type function of keys in @structure@
--    * @weakKey :: *@, a weakend lower kinded equivalent to @key@ used for storing in regular 'Map'
--    * @value :: * -> *@, the type function of values in the initial @structure@ and its patches
--    * @value' :: * -> *@, the type function of result values produced by the user's traversal function and returned via the result @structure@
--    * @patch :: (* -> *) -> *@, parameterized by the value type function (e.g. @value@ or @value'@) which represent the type of patches which get applied to
--    the @structure@ as events are ingested
--    * @structure :: (* -> *) ->@, parameterized by the value type function (e.g. @value@ or @value'@) which represents the initial structure to build and
--    semantically represents the intervening structures produces by patching though the intervening structures are never actually computed here
--    * @t@, the Reflex timeline
--    * @m@, the monad that 'Adjustable' is being implemented, or the builder is being run in if not implementing 'Adjustable'.
--    * @u@, the monad @u@nder @m@ which implements something like 'Adjustable'.
--    * @b@, the base monad that @m@ runs on top of (and thus lifting is available from @b@ to @m@) and also is the monad that @m@ can request asynchronous
--    actions in using 'Requester'.
--    * @marker@, the type of nodes which can be installed in the view hierarchy to keep track of the edges of slots.
--    * @holder@, the type of values that can be used to carry a subtree of the view hierarchy around when not installed in the hierarchy proper.
--    * @slotAddendum@, some additional value that should be held on and associated loosely with the views installed in the slot. Used by UIKit to hold on to
--    action targets which would otherwise be released.
--
-- Addressing each of the complications:
--
--    1. A slot associated with an unready fragment still has a marker, which is later used by the commit function to install the view fragment if applicable.
--    2. Each slot keeps track of the most recent generation that become ready for it, so if a generation becomes ready that is now out of date it can avoid
--    stomping the previously installed views.
--    3. By consulting the slot's installed generation the top level never-ready count can be maintained. A slot's installed generation being 0 indicates never
--    installed, so if installing a new generation in that slot the never-ready count can be decremented.
--    4. By keeping the never-ready count and only marking the parent ready when it hits 0.
--    5. @adjustingBuilder@ takes in a couple functions (@foldM@ and @map@) which it uses to manipulate the generic @structure@. @structure@ is taken to be of
--    the dependent form @structure v@ where @v :: * -> *@. For @'DMap' k v@, @structure ~ 'DMap' k@. For 'Map', @structure ~ Compose (Map k) (Const v)@.
--    6. @adjustingBuilder@ takes in a couple functions (@foldM@ and @map@, like the structure) which manipulate the generic @patch@, just like @structure@.
--    One important difference though is that the @foldPatchM@ function exposes the action of each contained patch entry as a 'NodeInfo'.
--    7. The slot keeps the set of objects to retain associated with an installed generation.
--    8. Each commit function doesn't close on the original key it was associated with, but instead a reference to the key it should associate with when it
--    commits, or Nothing. When move or delete patches are processed, a map of keys to those references is updated when the new key (for a move) or Nothing
--    (for a delete).
{-# INLINE adjustingBuilder #-}
adjustingBuilder
  :: forall
       (key       ::        * -> *)
       (weakKey   ::             *)
       (value     ::        * -> *)
       (value'    ::        * -> *)
       (patch     :: (* -> *) -> *)
       (structure :: (* -> *) -> *)
       t m u b marker holder slotAddendum.
     ( MonadFix m, MonadHold t m, MonadIO m, NotReady t m, Requester t m, Request m ~ b
     , MonadIO u
     , MonadIO b
     , Ord weakKey)
  => AdjustingBuilderConfig m u b marker holder slotAddendum
  -- ^ Functions that @adjustingBuilder@ uses to manipulate the underlying view hierarchy.
  -> (forall (v :: * -> *) (v' :: * -> *). (forall a. key a -> v a -> u (v' a)) -> structure v -> Event t (patch v) -> u (structure v', Event t (patch v')))
  -- ^ The adjustment traversal for the structure in the layer underneath @m@, @u@.
  -> (forall a. key a -> weakKey)
  -- ^ Function for weakening a @key a@ into a weaker form @weakKey@.
  -> (forall (accum :: *) (v :: * -> *) n. Monad n => (forall a. accum -> key a -> v a -> n accum) -> accum -> structure v -> n accum)
  -- ^ Effectful folding function for keys and values of @structure@.
  -> (forall (v :: * -> *) (v' :: * -> *). (forall a. v a -> v' a) -> structure v -> structure v')
  -- ^ Plain old mapping function for @structure@.
  -> (forall (accum :: *) (v :: * -> *) n. Monad n => (forall a. accum -> key a -> NodeInfo key v a -> n accum) -> accum -> patch v -> n accum)
  -- ^ Effectful folding function for keys and patch node information of @patch@.
  -> (forall (v :: * -> *) (v' :: * -> *). (forall a. v a -> v' a) -> patch v -> patch v')
  -- ^ Plain old mapping function for @patch@.
  -> (forall a. key a -> value a -> m (value' a))
  -- ^ The child building function applied for each element of the structure and for new elements as patches arrive.
  -> structure value
  -- ^ The initial structure.
  -> Event t (patch value)
  -- ^ Patches to produce new generations of the structure.
  -> m (structure value', Event t (patch value'))
adjustingBuilder (AdjustingBuilderConfig {..}) adjustBase weakenKey foldStructureM mapStructure foldPatchM mapPatch =
  \ f structure0 structure' -> do

  let liftBaseFully :: forall a. b a -> m a
      liftBaseFully = _adjustingBuilderConfig_lift . _adjustingBuilderConfig_liftBase

  becameReadyInParent <- _adjustingBuilderConfig_becameReadyInParent

  -- create a marker to go at the very end, for the last child to use as a delimiter rather than the next slot's start
  endMarker <- liftBaseFully _adjustingBuilderConfig_newMarker

  let createSlot :: b (Slot slotAddendum marker)
      createSlot = do
        _slot_startMarker <- _adjustingBuilderConfig_newMarker
        _slot_addendum    <- _adjustingBuilderConfig_newSlotAddendum
        let _slot_installedGeneration = 0
        pure $ Slot {..}

      createSlot0 :: forall a. Map weakKey (Slot slotAddendum marker) -> key a -> value a -> b (Map weakKey (Slot slotAddendum marker))
      createSlot0 m key _ = do
        slot <- createSlot
        pure $ Map.insert (weakenKey key) slot m
  -- create slots for each key in the initial generation
  slots0 <- liftBaseFully $ foldStructureM createSlot0 mempty structure0
  slotsRef <- liftIO $ newIORef slots0

  -- functions for manipulating slots stored in slotsRef
  let
      -- get the Slot associated with a @weakKey@
      getSlot :: MonadIO n => weakKey -> n (Maybe (Slot slotAddendum marker))
      getSlot weakKey = Map.lookup weakKey <$> liftIO (readIORef slotsRef)

      -- put the Slot associated with a @weakKey@
      putSlot :: MonadIO n => weakKey -> Slot slotAddendum marker -> n ()
      putSlot weakKey slot = liftIO . modifyIORef' slotsRef $ Map.insert weakKey slot

  -- keep a mapping of keys to IORef used by the commit functions to look up which slot they should go in, so that unready fragments get installed in the right
  -- place after moves/deletes. see complication (8) above
  unreadyKeyReferencesRef :: IORef (Map weakKey (IORef (Maybe weakKey))) <- liftIO $ newIORef mempty

  -- keep track of how many slots have never been ready. when it hits 0 for the first time, mark the parent ready
  neverBeenReadyCountRef :: IORef Word <- liftIO $ newIORef (error "neverBeenReadyCount not yet ready")
  hasEverBeenReadyRef :: IORef Bool <- liftIO $ newIORef False

  -- helper functions for doing the various view manipulations
  let -- helper to get the marker view immediately following a slot by its key, either the start marker of the next slot or the end marker
      markerAfter :: MonadIO n => weakKey -> n marker
      markerAfter key = maybe endMarker (_slot_startMarker . snd) . Map.lookupGT key <$> liftIO (readIORef slotsRef)

      -- get the Slot associated with a @weakKey@ or make a new one and add it to the map if one doesn't already exist
      getOrCreateSlotAndInstallMarker :: weakKey -> b (Slot slotAddendum marker)
      getOrCreateSlotAndInstallMarker weakKey =
        getSlot weakKey >>= \ case
          Just slot -> pure slot
          Nothing -> do
            slot <- createSlot
            putSlot weakKey slot
            nextMarker <- markerAfter weakKey
            _adjustingBuilderConfig_insertMarkerBeforeMarker (_slot_startMarker slot) nextMarker
            pure slot

      -- do the final steps to install a fragment in a slot, either after a commit has determined it's okay to do or if the fragment was immediately ready
      install :: weakKey -> Slot slotAddendum marker -> Bool -> MovingSlot slotAddendum holder -> b ()
      install key slot committing (MovingSlot {..}) = do
        let slot' = slot
              { _slot_addendum            = _movingSlot_addendum
              , _slot_installedGeneration = _movingSlot_generation
              }
        putSlot key slot'
        liftIO $ modifyIORef' unreadyKeyReferencesRef (Map.delete key)
        nextMarker <- markerAfter key
        _adjustingBuilderConfig_replaceBetweenMarkersWithHolder (_slot_startMarker slot) nextMarker _movingSlot_holder

        -- (3)
        when (_slot_installedGeneration slot == 0 && committing) previouslyNeverReadySlotBecameReady

      -- a slot which has never been installed (i.e. _slot_installedGeneration == 0) has become ready
      previouslyNeverReadySlotBecameReady :: b ()
      previouslyNeverReadySlotBecameReady = do
        neverBeenReadyCount <- liftIO $ readIORef neverBeenReadyCountRef
        hasEverBeenReady <- liftIO $ readIORef hasEverBeenReadyRef
        when (neverBeenReadyCount > 0) . liftIO . writeIORef neverBeenReadyCountRef $! pred neverBeenReadyCount
        -- (4)
        when (neverBeenReadyCount == 0 && not hasEverBeenReady) $ do
          becameReadyInParent
          liftIO $ writeIORef hasEverBeenReadyRef True

  -- committing a child is fiddly. this is discussed in more detail in the main documentation above. in particular (1), (2), (3), (4), (7), (8) all directly
  -- interact with the commit callback.
  let commitChild :: IORef (Maybe weakKey) -> MovingSlot slotAddendum holder -> b ()
      commitChild keyMayRef movingSlot@(MovingSlot {..})= do
        -- (8)
        keyMay <- liftIO $ readIORef keyMayRef
        for_ keyMay $ \ key -> do
          slotMay <- getSlot key
          for_ slotMay $ \ slot -> do
            -- (2)
            unless (_movingSlot_generation > _slot_installedGeneration slot) $ do
              install key slot True movingSlot


  -- delegate to the base monad to traverse runChildInstance on each initial child in the structure along with each updated child from the structure patches
  -- run the action against each new or updated child, then do the actual view manipulation later based on the results of the individual builds
  let structureGeneration0 :: structure (WithGeneration value)
      structureGeneration0 = mapStructure (WithGeneration 1) structure0
  structureGeneration' :: Event t (patch (WithGeneration value)) <-
    fmap (\ (gen, patch) -> mapPatch (WithGeneration gen) patch) <$> numberOccurrencesFrom 2 structure'

  (children0 :: structure (ChildInstance slotAddendum holder value'), children' :: Event t (patch (ChildInstance slotAddendum holder value'))) <-
    let runChildInstance :: forall a. key a -> WithGeneration value a -> u (ChildInstance slotAddendum holder value' a)
        runChildInstance k (WithGeneration generation v) = do
          let _movingSlot_generation = generation
              _childInstance_generation = generation
              weakKey = weakenKey k
          _movingSlot_holder <- _adjustingBuilderConfig_liftBase _adjustingBuilderConfig_newHolder
          _movingSlot_addendum <- _adjustingBuilderConfig_liftBase _adjustingBuilderConfig_newSlotAddendum
          let movingSlot = MovingSlot {..}
          keyReference <- liftIO . newIORef $ Just weakKey
          -- actually run the user's action in an isolated environment
          (_childInstance_result, isReady) <-
            _adjustingBuilderConfig_runChild _movingSlot_holder _movingSlot_addendum (commitChild keyReference movingSlot) (f k v)
          _childInstance_ready <-
            if isReady
              -- when ready now, pass back out the view holder and retain objs for the next step to pick up
              then pure $ Just (MovingSlot {..})
              -- when not ready now, keep a keyed reference to the key reference (ha) for moves or deletes which might happen prior to becoming ready, see (8)
              else liftIO $ modifyIORef' unreadyKeyReferencesRef (Map.insert weakKey keyReference) $> Nothing
          pure $ ChildInstance {..}

    in _adjustingBuilderConfig_lift $ adjustBase runChildInstance structureGeneration0 structureGeneration'

  -- handle the results initial generation by installing each placeholder in the parent view along with each child fragment that's ready now
  -- if any children were not yet ready, let childCommit handle inserting the views later and count them now
  neverBeenReadyCount0 <-
    let processInitialChild :: forall a. Word -> key a -> ChildInstance slotAddendum holder value' a -> m Word
        processInitialChild prevCount key (ChildInstance {..}) = do
          let weakKey = weakenKey key
          slot@(Slot { _slot_startMarker }) <- fromMaybe (error "an initial slot wasn't present at initialization time?!") <$> getSlot weakKey
          _adjustingBuilderConfig_appendMarker _slot_startMarker
          case _childInstance_ready of
            Nothing -> pure $ succ prevCount
            Just (MovingSlot {..}) -> do
              putSlot weakKey $ slot
                { _slot_installedGeneration = 1
                , _slot_addendum = _movingSlot_addendum
                }
              _adjustingBuilderConfig_appendHolder _movingSlot_holder
              pure prevCount
    in foldStructureM processInitialChild 0 children0
  liftIO $ writeIORef neverBeenReadyCountRef neverBeenReadyCount0
  when (neverBeenReadyCount0 > 0) notReady

  _adjustingBuilderConfig_appendMarker endMarker

  -- handle subsequent generations by interpreting the patch contents into various hierarchy affecting actions
  _ <-
    let processUpdatePatch :: patch (ChildInstance slotAddendum holder value') -> b ()
        processUpdatePatch patch = do
          -- go through and pick up any slot contents that are about to be moved elsewhere. in the next step, we'll do the main insert/update/delete work but
          -- in order to avoid conflicts where a move operation needs the contents of a slot which has since been replaced or deleted by another operation
          -- we'll have to grab the contents of the source slots now
          collected <-
            let collectMovingFragments
                  :: forall a. (Map weakKey (MovingSlot slotAddendum holder))
                  -> key a
                  -> NodeInfo key (ChildInstance slotAddendum holder value') a
                  -> b (Map weakKey (MovingSlot slotAddendum holder))
                collectMovingFragments collected key (NodeInfo _ (ComposeMaybe toMay))
                  | Just _ <- toMay = do
                      let weakKey = weakenKey key
                      slotMay <- getSlot weakKey
                      case slotMay of
                        Just (Slot {..}) -> do
                          nextMarker <- markerAfter weakKey
                          _movingSlot_holder <- _adjustingBuilderConfig_collectViewsBetween _slot_startMarker nextMarker
                          let movingSlot = MovingSlot
                                { _movingSlot_addendum   = _slot_addendum
                                , _movingSlot_generation = _slot_installedGeneration
                                , _movingSlot_holder
                                }
                          pure $ Map.insert weakKey movingSlot collected
                        Nothing ->
                          pure collected
                  | otherwise = pure collected
            in foldPatchM collectMovingFragments mempty patch

          -- now that we've collected up all the fragments we'll need for moves, it's safe to proceed with inserting/updating, deleting, and placing moved
          -- fragments along with any inserting or deleting of slots that's required
          _ <-
            let applyUpdate
                  :: forall a. ()
                  -> key a
                  -> NodeInfo key (ChildInstance slotAddendum holder value') a
                  -> b ()
                applyUpdate _ key (NodeInfo from _) = do
                  let weakKey = weakenKey key
                  case from of
                    From_Insert (ChildInstance {..}) -> do
                      slot <- getOrCreateSlotAndInstallMarker weakKey
                      case _childInstance_ready of
                        Nothing -> do
                          -- (3) if this is a new slot entirely and not updating an existing slot, then make sure to increment the neverBeenReadyCount to
                          -- prevent the parent from becoming ready while this new unready slot is still pending. (4) is not a concern because we have a
                          -- separate bool keeping track of whether we've marked the parent ready or not yet
                          liftIO . when (_slot_installedGeneration slot == 0) $ modifyIORef' neverBeenReadyCountRef succ

                          -- nothing else to do, the start marker has already been installed and the commit hook has been set up so that'll install the view
                          -- when it's ready

                        Just movingSlot -> do
                          install weakKey slot True movingSlot

                    From_Move srcKey -> do
                      let weakSrcKey = weakenKey srcKey
                          movingSlot = fromMaybe (error "moving slot not in collected map, patch invariant violated") (Map.lookup weakSrcKey collected)
                      slot <- getOrCreateSlotAndInstallMarker weakKey
                      install weakKey slot False movingSlot

                    From_Delete -> do
                      slotMay <- getSlot weakKey
                      for_ slotMay $ \ (Slot {..}) -> do
                        nextMarker <- markerAfter weakKey
                        _adjustingBuilderConfig_deleteViewsBetween _slot_startMarker nextMarker
                        _adjustingBuilderConfig_removeMarker _slot_startMarker
                        -- make sure to decrement the never ready count if this was a slot which was never installed before it was deleted
                        when (_slot_installedGeneration == 0) previouslyNeverReadySlotBecameReady
                      liftIO $ modifyIORef' slotsRef (Map.delete weakKey)
                      liftIO $ modifyIORef' unreadyKeyReferencesRef (Map.delete weakKey)
            in foldPatchM applyUpdate () patch

          pure ()

    in requesting_ $ processUpdatePatch <$> children'

  pure $ (mapStructure _childInstance_result children0, mapPatch _childInstance_result <$> children')

-- | Structure which adapts 'adjustingBuilder' to some specific underlying view hierarchy.
--
-- Type parameters:
--
--     * @m@ the monad 'adjustingBuilder' runs in, e.g. @ImmediateDomBuilderT@ or @UIKitViewBuilderT@
--     * @u@ the monad under @m@
--     * @b@ the base monad under @u@ where asynchronous processing executes, e.g. @IO@ or @MainThread@
--     * @marker@ the type of marker nodes in the underlying view hierarchy, e.g. @DOM.Text@ or @UIView@
--     * @holder@ the type of nodes which carry around view nodes, e.g. @DOM.DocumentFragment@ or @UIView@
--     * @slotAddendum@ is whatever additional (mutable) data needs to be carried around with slots, nothing for DOM but required for UIKit
data AdjustingBuilderConfig m u b marker holder slotAddendum = AdjustingBuilderConfig
  { _adjustingBuilderConfig_liftBase :: forall a. b a -> u a
  -- ^ How to embed a base monad action @b a@ in the underlying monad @u@.
  , _adjustingBuilderConfig_lift :: forall a. u a -> m a
  -- ^ How to embed an underlying monad action @u a@ in the main monad @m@.
  , _adjustingBuilderConfig_newSlotAddendum :: b slotAddendum
  -- ^ How to create a slot addendum when about to run a child builder via @_adjustingBuildConfig_runChild@
  , _adjustingBuilderConfig_runChild :: forall a. holder -> slotAddendum -> b () -> m a -> u (a, Bool)
  -- ^ How to run a child builder in an isolated environment where views it builds are added to the given holder and using the given commit function if it
  -- becomes ready asynchronously. Returns @(result, isReady)@ where @isReady == True@ indicates that the child never invoked 'notReady' or 'notReadyUntil' and
  -- as such can be immediately installed. When @isReady == True@ the commit function should never be called. The commit function must never be called more
  -- than once.
  , _adjustingBuilderConfig_becameReadyInParent :: m (b ())
  -- ^ How to capture an action in the base monad which marks the parent ready, converse to 'notReady'.
  , _adjustingBuilderConfig_newHolder :: b holder
  -- ^ How to create a new holder for a child instance.
  , _adjustingBuilderConfig_appendHolder :: holder -> m ()
  -- ^ How to append the views inside a holder to the current parent view of @m@. The holder can be made empty.
  , _adjustingBuilderConfig_collectViewsBetween :: marker -> marker -> b holder
  -- ^ How to collect all views between two markers into a new holder, removing them from the view hierarchy.
  , _adjustingBuilderConfig_deleteViewsBetween :: marker -> marker -> b ()
  -- ^ How to delete all views between two markers.
  , _adjustingBuilderConfig_replaceBetweenMarkersWithHolder :: marker -> marker -> holder -> b ()
  -- ^ How to replace all views between two markers with the contents of a holder. The holder can be made empty.
  , _adjustingBuilderConfig_newMarker :: b marker
  -- ^ How to create a new marker in the base monad.
  , _adjustingBuilderConfig_appendMarker :: marker -> m ()
  -- ^ How to append a marker to the current parent view of @m@.
  , _adjustingBuilderConfig_insertMarkerBeforeMarker :: marker -> marker -> b ()
  -- ^ How to place a marker immediately before some other marker.
  , _adjustingBuilderConfig_removeMarker :: marker -> b ()
  -- ^ How to remove a marker from the view hierarchy.
  }

-- | Structure holding information about a single key slot in an 'adjustingBuilder' installation. Does not hold a reference to the actual views, those are
-- presumed to be held by the underlying view hierarchy.
data Slot addendum marker = Slot
  { _slot_addendum            :: !addendum
  -- ^ Additional data to carry around and refer to with the slot, such as additional view objects that need to be retained.
  , _slot_installedGeneration :: {-# UNPACK #-} !Word
  -- ^ What generation number is installed in the slot presently, to avoid older versions committing later than new versions and replacing the new incorrectly.
  , _slot_startMarker         :: !marker
  -- ^ The marker view which locates the beginning of the slot.
  }

-- | Structure holding a slot's contents which are not presently installed, either because it's after the child builder has run but before it's been installed
-- or because a slot's contents are moving from one key to another.
data MovingSlot addendum holder = MovingSlot
  { _movingSlot_addendum   :: !addendum
  -- ^ Additional data to carry around and refer to with the slot, such as additional view objects that need to be retained.
  , _movingSlot_generation :: {-# UNPACK #-} !Word
  -- ^ What generation the views came from.
  , _movingSlot_holder     :: !holder
  -- ^ The views, held by a throwaway container view.
  }

-- | An instance of a child being built, either from the first or later generations. Carries information from the child build step to the intitial generation
-- processing or later generation event handling.
data ChildInstance addendum holder v a = ChildInstance
  { _childInstance_ready      :: !(Maybe (MovingSlot addendum holder))
  -- ^ If the build was immediately ready (i.e. never used 'notReady' or 'notReadyUntil') then contains the slot contents to be installed as a 'MovingSlot'.
  -- @Nothing@ indicates not ready yet and the commit handler will install.
  , _childInstance_generation :: {-# UNPACK #-} !Word
  -- ^ What generation was built. 0 is invalid, 1 is the initial generation, 2 is the first event updating the build, and so on.
  , _childInstance_result     :: v a
  -- ^ The user result returned by the builder to hand back out.
  }

-- | Composes some @v a@ with what generation it's for. Used by 'adjustingBuilder' to pass the generation to the child building step.
data WithGeneration v a = WithGeneration
  { _withGeneration_generation :: {-# UNPACK #-} !Word
  -- ^ The generation the value is associated with.
  , _withGeneration_value      :: v a
  -- ^ Whatever value to carry.
  }

-- | Flipped version of 'withSome'.
foldSome :: (forall a. f a -> r) -> Some f -> r
foldSome f s = withSome s f

insertNodeInfo :: v a -> NodeInfo k v a
insertNodeInfo va = NodeInfo (From_Insert va) (ComposeMaybe Nothing)

deleteNodeInfo :: NodeInfo k v a
deleteNodeInfo = NodeInfo From_Delete (ComposeMaybe Nothing)

{-# INLINABLE runWithReplaceImpl #-}
runWithReplaceImpl
  :: forall value value' t m u b marker holder slotAddendum.
     ( MonadFix m, MonadHold t m, MonadIO m, NotReady t m, Requester t m, Request m ~ b
     , Adjustable t u, MonadIO u
     , MonadIO b
     )
  => AdjustingBuilderConfig m u b marker holder slotAddendum
  -> m value -> Event t (m value')
  -> m (value, Event t value')
runWithReplaceImpl config child0 child' = do
  -- the use of Either is a bit of a hack to work around runWithReplace uniquely allowing a different type for the initial build versus later ones, where all
  -- the other flavors are consistent.
  (e0, e') <- adjustingBuilder
    {- key            -} @(Const ())
    {- weakKey        -} @()
    {- value          -} @(Const (Either (m value) (m value')))
    {- value'         -} @(Const (Either value value'))
    {- patch          -} @Some
    {- structure      -} @Some
    {- config         -} config
    {- adjustBase     -} adjustBase
    {- weakenKey      -} (\ _ -> ())
    {- foldStructureM -} (\ f z -> foldSome (f z key))
    {- mapStructure   -} (\ f -> foldSome (Some.This . f))
    {- foldPatchM     -} (\ f z -> foldSome (f z key . insertNodeInfo))
    {- mapPatch       -} (\ f -> foldSome (Some.This . f))
    {- f              -} f'
    {- structure0     -} (Some.This . Const . Left $ child0)
    {- structure'     -} (Some.This . Const . Right <$> child')

  pure
    ( foldSome (\ (Const e) -> either id (error "initial build should definitely have been Left") e) e0
    , foldSome (\ (Const e) -> either (error "subsequent builds should definitely be Right") id e) <$> e'
    )
  where
    key = Const ()

    adjustBase
      :: forall (v :: * -> *) (v' :: * -> *).
         (forall a. Const () a -> v a -> u (v' a))
      -> Some v -> Event t (Some v) -> u (Some v', Event t (Some v'))
    adjustBase g v0 v' =
      runWithReplace (withSome v0 (fmap Some.This . g key)) (foldSome (fmap Some.This . g key) <$> v')

    f' :: forall a. Const () a -> Const (Either (m value) (m value')) a -> m (Const (Either value value') a)
    f' _ (Const (Left  mv )) = Const . Left  <$> mv
    f' _ (Const (Right mv')) = Const . Right <$> mv'

{-# INLINABLE mapIntMapPatchWithKey #-}
-- |Map a function @Int -> a -> b@ over all @a@s in the given @'PatchIntMap' a@ (that is, all inserts/updates), producing a @PatchIntMap b@.
mapIntMapPatchWithKey :: (Int -> a -> b) -> PatchIntMap a -> PatchIntMap b
mapIntMapPatchWithKey f (PatchIntMap m) = PatchIntMap $ IntMap.mapWithKey (\ k mv -> f k <$> mv) m

{-# INLINABLE traverseIntMapWithKeyWithAdjustImpl #-}
traverseIntMapWithKeyWithAdjustImpl
  :: forall (value :: *) (value' :: *) t m u b marker holder slotAddendum.
     ( MonadFix m, MonadHold t m, MonadIO m, NotReady t m, Requester t m, Request m ~ b
     , Adjustable t u, MonadIO u
     , MonadIO b
     )
  => AdjustingBuilderConfig m u b marker holder slotAddendum
  -> (IntMap.Key -> value -> m value')
  -> IntMap value -> Event t (PatchIntMap value)
  -> m (IntMap value', Event t (PatchIntMap value'))
traverseIntMapWithKeyWithAdjustImpl config f im0 im' =
  bimap (IntMap.map (foldSome getConst) . getCompose) (fmap (mapIntMapPatchWithKey (\ _ -> foldSome getConst) . getCompose)) <$> adjustingBuilder
    {- key            -} @(Const IntMap.Key)
    {- weakKey        -} @IntMap.Key -- FIXME? ideally adjustingBuilder would work in terms of IntMap instead of Map IntMap.Key
    {- value          -} @(Const value)
    {- value'         -} @(Const value')
    {- patch          -} @(Compose PatchIntMap Some)
    {- structure      -} @(Compose IntMap Some)
    {- config         -} config
    {- adjustBase     -} adjustBase
    {- weakenKey      -} getConst
    {- foldStructureM -} (\ g z -> foldlM (\ accum (k, v) -> withSome v (g accum (Const k))) z . IntMap.assocs . getCompose)
    {- mapStructure   -} (\ g -> Compose . IntMap.map (foldSome (Some.This . g)) . getCompose)
    {- foldPatchM     -} foldPatchM
    {- mapPatch       -} (\ g -> Compose . mapIntMapPatchWithKey (\ _ -> foldSome (Some.This . g)) . getCompose)
    {- f              -} (\ (Const k) (Const v) -> Const <$> f k v)
    {- structure0     -} (Compose (Some.This . Const <$> im0))
    {- structure'     -} (Compose . fmap (Some.This . Const) <$> im')
  where
    adjustBase
      :: forall (v :: * -> *) (v' :: * -> *).
         (forall a. Const IntMap.Key a -> v a -> u (v' a))
      -> Compose IntMap Some v -> Event t (Compose PatchIntMap Some v)
      -> u (Compose IntMap Some v', Event t (Compose PatchIntMap Some v'))
    adjustBase g s0 s' =
      let g' k = foldSome (fmap Some.This . g (Const k))
      in bimap Compose (fmap Compose) <$> traverseIntMapWithKeyWithAdjust g' (getCompose s0) (getCompose <$> s')

    foldPatchM
      :: forall accum (v :: * -> *) n. Monad n => (forall a. accum -> Const IntMap.Key a -> NodeInfo (Const IntMap.Key) v a -> n accum)
      -> accum -> Compose PatchIntMap Some v -> n accum
    foldPatchM g z (Compose (PatchIntMap pim)) =
      let g' :: accum -> (IntMap.Key, Maybe (Some v)) -> n accum
          g' accum (k, Nothing             ) = g accum (Const k) deleteNodeInfo
          g' accum (k, (Just (Some.This v))) = g accum (Const k) (insertNodeInfo v)
      in  foldlM g' z (IntMap.assocs pim)

{-# INLINABLE foldDMapM #-}
foldDMapM
  :: forall (accum :: *) (k :: * -> *) (v :: * -> *) m. Monad m
  => (forall a. accum -> k a -> v a -> m accum)
  -> accum -> DMap k v -> m accum
foldDMapM f z =
  foldlM (\ accum (k :=> v) -> f accum k v) z . DMap.toList

{-# INLINABLE foldPatchDMapM #-}
foldPatchDMapM
  :: forall (accum :: *) (k :: * -> *) (v :: * -> *) m. Monad m
  => (forall a. accum -> k a -> NodeInfo k v a -> m accum)
  -> accum -> PatchDMap k v -> m accum
foldPatchDMapM f z (PatchDMap pdm) =
  foldDMapM f z . DMap.map (\ (ComposeMaybe mv) -> maybe deleteNodeInfo insertNodeInfo mv) $ pdm

{-# INLINABLE traverseDMapWithKeyWithAdjustImpl #-}
traverseDMapWithKeyWithAdjustImpl
  :: forall (key :: * -> *) (value :: * -> *) (value' :: * -> *) t m u b marker holder slotAddendum.
     ( MonadFix m, MonadHold t m, MonadIO m, NotReady t m, Requester t m, Request m ~ b
     , Adjustable t u, MonadIO u
     , MonadIO b
     , GCompare key
     )
  => AdjustingBuilderConfig m u b marker holder slotAddendum
  -> (forall a. key a -> value a -> m (value' a))
  -> DMap key value -> Event t (PatchDMap key value)
  -> m (DMap key value', Event t (PatchDMap key value'))
traverseDMapWithKeyWithAdjustImpl config f dm0 dm' =
  adjustingBuilder
    {- key            -} @key
    {- weakKey        -} @(Some key)
    {- value          -} @value
    {- value'         -} @value'
    {- patch          -} @(PatchDMap key)
    {- structure      -} @(DMap key)
    {- config         -} config
    {- adjustBase     -} traverseDMapWithKeyWithAdjust
    {- weakenKey      -} Some.This
    {- foldStructureM -} foldDMapM
    {- mapStructure   -} DMap.map
    {- foldPatchM     -} foldPatchDMapM
    {- mapPatch       -} mapPatchDMap
    {- f              -} f
    {- structure0     -} dm0
    {- structure'     -} dm'

{-# INLINABLE traverseDMapWithKeyWithAdjustWithMoveImpl #-}
traverseDMapWithKeyWithAdjustWithMoveImpl
  :: forall (key :: * -> *) (value :: * -> *) (value' :: * -> *) t m u b marker holder slotAddendum.
     ( MonadFix m, MonadHold t m, MonadIO m, NotReady t m, Requester t m, Request m ~ b
     , Adjustable t u, MonadIO u
     , MonadIO b
     , GCompare key
     )
  => AdjustingBuilderConfig m u b marker holder slotAddendum
  -> (forall a. key a -> value a -> m (value' a))
  -> DMap key value -> Event t (PatchDMapWithMove key value)
  -> m (DMap key value', Event t (PatchDMapWithMove key value'))
traverseDMapWithKeyWithAdjustWithMoveImpl config f dm0 dm' =
  adjustingBuilder
    {- key            -} @key
    {- weakKey        -} @(Some key)
    {- value          -} @value
    {- value'         -} @value'
    {- patch          -} @(PatchDMapWithMove key)
    {- structure      -} @(DMap key)
    {- config         -} config
    {- adjustBase     -} traverseDMapWithKeyWithAdjustWithMove
    {- weakenKey      -} Some.This
    {- foldStructureM -} foldDMapM
    {- mapStructure   -} DMap.map
    {- foldPatchM     -} (\ g z -> foldDMapM g z . unPatchDMapWithMove)
    {- mapPatch       -} mapPatchDMapWithMove
    {- f              -} f
    {- structure0     -} dm0
    {- structure'     -} dm'

