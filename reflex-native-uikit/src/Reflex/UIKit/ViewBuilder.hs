{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |Implementation of 'ViewBuilder' for UIKit, implementing all essential view hierarchy creation and maintenance functionality.
module Reflex.UIKit.ViewBuilder
  (
  -- * External interface
    UIKitViewBuilderSpace, UIKitViewBuilderT(..), SupportsUIKitViewBuilder, liftMainThread, runUIKitViewBuilderT
  -- * Primitive operations and internal state
  , UIKitRequesterT, Env(..), BuildFrame(..), becameReady, append, appendRetainObj, appendFragment, addGestureRecognizer, runSubviewBuilder
  -- * Implementation details
  , uikitViewBuilderConfig, uikitRunWithReplace, uikitTraverseIntMapWithKeyWithAdjust, uikitTraverseDMapWithKeyWithAdjust
  , uikitTraverseDMapWithKeyWithAdjustWithMove
  , readPanGestureRecognizer
  -- * Raw FFI bindings
  ) where

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Concurrent.Chan (Chan)
import Control.Monad (Monad, (=<<), (>>=), (<=<), when)
import Control.Monad.Exception (MonadAsyncException, MonadException)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad(type PrimState, primitive))
import Control.Monad.Ref (MonadAtomicRef(atomicModifyRef), MonadRef(type Ref, newRef, readRef, writeRef, modifyRef, modifyRef'))
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (DSum)
import Data.FastMutableIntMap (PatchIntMap(..))
import Data.Foldable (for_)
import Data.Functor (Functor(fmap), (<$), void)
import Data.Functor.Identity (Identity(..))
import Data.GADT.Compare (GCompare)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word (Word)
import Foreign.Ptr (Ptr)
import Foundation (NSMutableArray)
import qualified Foundation.NSMutableArray as NSMutableArray
import qualified Foundation.NSString as NSString
import ObjC (Obj, ObjPtr, ObjType, SafeObjCoerce, asObj, downCastObj, retainObj, withObjPtr)
import Reflex
  ( Adjustable(runWithReplace, traverseDMapWithKeyWithAdjust, traverseIntMapWithKeyWithAdjust, traverseDMapWithKeyWithAdjustWithMove)
  , Event
  , MonadHold(hold, holdDyn, holdIncremental, buildDynamic, headE)
  , MonadSample(sample)
  , Reflex
  , ffor, headE
  )
import Reflex.Host.Class (MonadReflexCreateTrigger(newEventWithTrigger, newFanEventWithTrigger))
import Reflex.Native.AdjustingBuilder (AdjustingBuilderConfig(..))
import qualified Reflex.Native.AdjustingBuilder as AdjustingBuilder
import Reflex.Native.Gesture (GestureSpec(..), GestureState(..), PanGesture(..))
import Reflex.Native.TextConfig (TextConfig(..))
import Reflex.Native.ViewBuilder.Class
  ( ViewBuilder(type ViewBuilderSpace, buildTextView, buildView, placeRawView, wrapRawView, recognizeGesture)
  , ViewSpace(type RawTextView, type RawView), TextView(..), View(..)
  )
import Reflex.Native.ViewConfig (RawViewConfig(..))
import Reflex.NotReady.Class (NotReady(notReady, notReadyUntil))
import Reflex.Patch.DMap (PatchDMap(..))
import Reflex.Patch.DMapWithMove (PatchDMapWithMove(..))
import Reflex.PerformEvent.Class (PerformEvent(type Performable, performEvent, performEvent_), performEventAsync)
import Reflex.PostBuild.Class (PostBuild(getPostBuild))
import Reflex.Requester.Base (RequesterT(..), runRequesterT, traverseRequesterData)
import Reflex.Requester.Class (Requester(type Request, type Response, requesting, requesting_))
import Reflex.TriggerEvent.Base (EventTriggerRef, TriggerEventT(..), runTriggerEventT, TriggerInvocation)
import Reflex.TriggerEvent.Class (TriggerEvent(newTriggerEvent, newTriggerEventWithOnComplete, newEventWithLazyTriggerWithOnComplete))
import Reflex.UIKit.Config (applyInitialViewConfig, applyModifyViewConfig)
import Reflex.UIKit.Conversions (pointFromCGPoint)
import Reflex.UIKit.Style (applyLabelStyle, applyViewStyle, initialStyle, modifyStyle)
import UIKit
  ( MainThread(..), UILabel, UIGestureRecognizerType, UIPanGestureRecognizer, UIView, UIViewType
  , asUIView, checkMainThread, dispatchAsyncMain
  )
import qualified UIKit.Generic.GestureRecognizerTarget as GestureRecognizerTarget
import qualified UIKit.Generic.View as GenericView
import qualified UIKit.UILabel as UILabel
import UIKit.UIGestureRecognizer (UIGestureRecognizerState(..))
import qualified UIKit.UIGestureRecognizer as UIGestureRecognizer
import qualified UIKit.UIPanGestureRecognizer as UIPanGestureRecognizer
import qualified UIKit.UIView as UIView


-- |Structure maintaining state for a frame on the nominal stack of builders. Each time a nested build that might be discarded occurs a new frame is "pushed"
-- by invoking a child builder action with a new @BuildFrame@, such as during @runWithReplace@ or other 'Adjustable' methods.
data BuildFrame = BuildFrame
  { _buildFrame_retainObjs :: {-# UNPACK #-} !NSMutableArray
  -- ^Mutable array of ObjC objs to keep alive while this frame is alive, during build time and while the built hierarchy is installed following build time.
  -- Usually action targets that would not otherwise be retained by the view hierarchy.
  , _buildFrame_unreadyChildren :: {-# UNPACK #-} !(IORef Word)
  -- ^Number of children in the current frame which have indicated 'notReady' (or 'notReadyUntil') and which might prevent this frame and/or its parent frame(s)
  -- from being installed.
  , _buildFrame_commitAction :: !(MainThread ())
  -- ^The action to execute when @_buildFrame_unreadyChildren@ goes to 0 after being nonzero which install the frame's built hierarchy.
  }

-- |The environment a 'UIKitViewBuilderT' runs in, referring to a 'BuildFrame' for the current adjustable frame and the parent view to append new views to.
--
-- The parent view varies when building subviews ala 'buildView' but the build frame does not, while conversely the build frame varies but the parent view
-- might not when performing 'Adjustable' methods or similar.
data Env = Env
  { _env_parentView :: {-# UNPACK #-} !UIView
  -- ^The view that views to 'append' should become subviews of.
  , _env_frame :: BuildFrame
  -- ^The current @Adjustable@ frame keeping track of readiness status and so on.
  }

-- |Run a 'UIKitViewBuilderT' build action adding views to the given parent but keeping the same 'BuildFrame'.
runSubviewBuilder :: (SupportsUIKitViewBuilder t m, SafeObjCoerce view UIViewType) => ObjPtr view -> UIKitViewBuilderT t m a -> UIKitViewBuilderT t m a
runSubviewBuilder view child = do
  env <- UIKitViewBuilderT ask
  let childEnv = env { _env_parentView = asUIView view }
  result <- UIKitViewBuilderT . lift $ runReaderT (unUIKitViewBuilderT child) childEnv
  pure result

-- |Type of 'ViewSpace' for UIKit with the usual UIKit types as the raw types, e.g. @RawView ~ UIView@.
data UIKitViewBuilderSpace

-- |Instance for UIKit.
instance ViewSpace UIKitViewBuilderSpace where
  type RawTextView UIKitViewBuilderSpace = UILabel
  type RawView UIKitViewBuilderSpace = UIView

-- |Constraints required of a monad to support a 'UIKitViewBuilderT'.
type SupportsUIKitViewBuilder t m =
  ( Reflex t
  , MonadIO m
  , MonadHold t m
  , MonadFix m
  , MonadReflexCreateTrigger t m
  , MonadRef m, Ref m ~ Ref IO
  , Adjustable t m
  , PrimMonad m
  )

-- |The monad underneath the @ReaderT@ in a 'UIKitViewBuilderT'.
type UIKitRequesterT t m = RequesterT t MainThread Identity (TriggerEventT t m)

-- |'ViewBuilder' monad for building view hierarchies using UIKit.
newtype UIKitViewBuilderT t m a = UIKitViewBuilderT
  { unUIKitViewBuilderT :: ReaderT Env (UIKitRequesterT t m) a
  }

deriving instance Functor m => Functor (UIKitViewBuilderT t m)
deriving instance Monad m => Applicative (UIKitViewBuilderT t m)
deriving instance Monad m => Monad (UIKitViewBuilderT t m)
deriving instance MonadFix m => MonadFix (UIKitViewBuilderT t m)
deriving instance MonadIO m => MonadIO (UIKitViewBuilderT t m)
deriving instance MonadException m => MonadException (UIKitViewBuilderT t m)
deriving instance MonadAsyncException m => MonadAsyncException (UIKitViewBuilderT t m)

-- |Pass through 'PrimMonad'.
instance PrimMonad m => PrimMonad (UIKitViewBuilderT t m) where
  type PrimState (UIKitViewBuilderT t m) = PrimState m
  primitive = lift . primitive

-- |Straightforward lift.
instance MonadTrans (UIKitViewBuilderT t) where
  lift = UIKitViewBuilderT . lift . lift . lift

-- |Pass through 'MonadAtomicRef'.
instance MonadAtomicRef m => MonadAtomicRef (UIKitViewBuilderT t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

-- |Pass through 'MonadHold'.
instance MonadHold t m => MonadHold t (UIKitViewBuilderT t m) where
  {-# INLINABLE hold #-}
  hold v0 v' = lift $ hold v0 v'
  {-# INLINABLE holdDyn #-}
  holdDyn v0 v' = lift $ holdDyn v0 v'
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 v' = lift $ holdIncremental v0 v'
  {-# INLINABLE buildDynamic #-}
  buildDynamic a0 = lift . buildDynamic a0
  {-# INLINABLE headE #-}
  headE = lift . headE

-- |Pass through 'MonadRef'.
instance MonadRef m => MonadRef (UIKitViewBuilderT t m) where
  type Ref (UIKitViewBuilderT t m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r
  {-# INLINABLE modifyRef #-}
  modifyRef r = lift . modifyRef r
  {-# INLINABLE modifyRef' #-}
  modifyRef' r = lift . modifyRef' r

-- |Pass through 'MonadReflexCreateTrigger'.
instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (UIKitViewBuilderT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

-- |Pass through 'MonadSample'.
instance MonadSample t m => MonadSample t (UIKitViewBuilderT t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

-- |Pass through 'PerformEvent'.
instance PerformEvent t m => PerformEvent t (UIKitViewBuilderT t m) where
  type Performable (UIKitViewBuilderT t m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ performEvent e

-- |Pass through 'PostBuild'.
instance PostBuild t m => PostBuild t (UIKitViewBuilderT t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift getPostBuild

-- |Pass through 'Requester'.
instance (Reflex t, Monad m) => Requester t (UIKitViewBuilderT t m) where
  type Request (UIKitViewBuilderT t m) = MainThread
  type Response (UIKitViewBuilderT t m) = Identity
  requesting = UIKitViewBuilderT . lift . requesting
  requesting_ = UIKitViewBuilderT . lift . requesting_

-- |Pass through 'TriggerEvent'.
instance (Monad m, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m) => TriggerEvent t (UIKitViewBuilderT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = UIKitViewBuilderT . lift . lift $ newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = UIKitViewBuilderT . lift . lift $ newTriggerEventWithOnComplete
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete f = UIKitViewBuilderT . lift . lift $ newEventWithLazyTriggerWithOnComplete f

-- |@ViewBuilder@ for UIKit.
instance SupportsUIKitViewBuilder t m => ViewBuilder t (UIKitViewBuilderT t m) where
  type ViewBuilderSpace (UIKitViewBuilderT t m) = UIKitViewBuilderSpace

  {-# INLINABLE buildTextView #-}
  buildTextView (TextConfig {..}) = do
    liftIO checkMainThread

    label <- liftMainThread $ do
      l <- UILabel.new
      applyInitialViewConfig l _textConfig_viewConfig
      applyLabelStyle initialStyle l _textConfig_initialStyle
      UILabel.setText l =<< liftIO (NSString.fromText _textConfig_initialText)
      pure l

    applyModifyViewConfig label _textConfig_viewConfig
    for_ _textConfig_modifyStyle $ applyLabelStyle modifyStyle label
    for_ _textConfig_setText $ requesting_ . fmap (UILabel.setText label <=< liftIO . NSString.fromText)

    append label

    pure $ TextView label

  {-# INLINABLE buildView #-}
  buildView cfg child = do
    liftIO checkMainThread

    view <- liftMainThread $ do
      (v, _) <- GenericView.new $ \ v -> do
        UIView.setAutoresizesSubviews v False
        UIView.setAutoresizingMask v 0
        pure $ GenericView.defaultViewConfig ()
      applyInitialViewConfig v cfg
      pure v

    applyModifyViewConfig view cfg

    result <- runSubviewBuilder view child

    append view

    pure (View view, result)

  {-# INLINABLE placeRawView #-}
  placeRawView = append

  {-# INLINABLE wrapRawView #-}
  wrapRawView view (RawViewConfig {..}) = do
    for_ _rawViewConfig_modifyStyle $ applyViewStyle modifyStyle view

    pure (View view)

  {-# INLINABLE recognizeGesture #-}
  recognizeGesture (View view) = \ case
    GestureSpec_Pan -> addGestureRecognizer view UIPanGestureRecognizer.new readPanGestureRecognizer

-- |Prevent installation of view hierarchies until replaced via 'Adjustable' or a given event fires.
instance SupportsUIKitViewBuilder t m => NotReady t (UIKitViewBuilderT t m) where
  {-# INLINABLE notReadyUntil #-}
  notReadyUntil trigger = do
    notReady
    first <- headE trigger
    env <- UIKitViewBuilderT ask
    requesting_ $ becameReady (_env_frame env) <$ first

  {-# INLINABLE notReady #-}
  notReady = do
    Env { _env_frame = BuildFrame {..} } <- UIKitViewBuilderT ask
    liftIO $ modifyIORef' _buildFrame_unreadyChildren succ

-- |Helper for decrementing the unready children counter of a build frame and possibly triggering the commit action.
{-# INLINE becameReady #-}
becameReady :: BuildFrame -> MainThread ()
becameReady (BuildFrame {..}) = do
  new <- liftIO $ pred <$> readIORef _buildFrame_unreadyChildren
  liftIO $ writeIORef _buildFrame_unreadyChildren $! new
  when (new == 0) _buildFrame_commitAction

-- |"Reflex.Native.AdjustingBuilder" configured for UIKit.
instance SupportsUIKitViewBuilder t m => Adjustable t (UIKitViewBuilderT t m) where
  {-# INLINABLE runWithReplace #-}
  runWithReplace = uikitRunWithReplace

  {-# INLINABLE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust = uikitTraverseIntMapWithKeyWithAdjust

  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust = uikitTraverseDMapWithKeyWithAdjust

  {-# INLINABLE traverseDMapWithKeyWithAdjustWithMove #-}
  traverseDMapWithKeyWithAdjustWithMove = uikitTraverseDMapWithKeyWithAdjustWithMove

-- |Raw FFI binding to create a new marker view for "Reflex.Native.AdjustingBuilder"
foreign import ccall unsafe uikitViewBuilder_newMarkerView :: MainThread (Ptr UIViewType)
-- |Raw FFI binding to create a new holder view for "Reflex.Native.AdjustingBuilder"
foreign import ccall unsafe uikitViewBuilder_newHolderView :: MainThread (Ptr UIViewType)
-- |Raw FFI binding to collect up views between some markers for "Reflex.Native.AdjustingBuilder"
foreign import ccall unsafe uikitViewBuilder_collectViewsBetween :: Ptr UIViewType -> Ptr UIViewType -> MainThread (Ptr UIViewType)
-- |Raw FFI binding to delete views between some markers for "Reflex.Native.AdjustingBuilder"
foreign import ccall unsafe uikitViewBuilder_deleteViewsBetween :: Ptr UIViewType -> Ptr UIViewType -> MainThread ()
-- |Raw FFI binding to replace the views between some markers with views in a holder for "Reflex.Native.AdjustingBuilder"
foreign import ccall unsafe uikitViewBuilder_replaceBetweenMarkersWithHolder :: Ptr UIViewType -> Ptr UIViewType -> Ptr UIViewType -> MainThread ()
-- |Raw FFI binding to install a marker prior to another marker for "Reflex.Native.AdjustingBuilder"
foreign import ccall unsafe uikitViewBuilder_insertMarkerBeforeMarker :: Ptr UIViewType -> Ptr UIViewType -> MainThread ()

-- |'AdjustingBuilderConfig' to adapt "Reflex.Native.AdjustingBuilder" to UIKit using the raw FFI bindings.
{-# INLINE uikitViewBuilderConfig #-}
uikitViewBuilderConfig :: MonadIO m => AdjustingBuilderConfig (UIKitViewBuilderT t m) (UIKitRequesterT t m) MainThread UIView UIView NSMutableArray
uikitViewBuilderConfig = AdjustingBuilderConfig {..}
  where
    _adjustingBuilderConfig_liftBase            = liftIO . unsafeUnMainThread -- just as (un)safe as 'liftMainThread'
    _adjustingBuilderConfig_lift                = UIKitViewBuilderT . lift
    _adjustingBuilderConfig_newSlotAddendum     = liftIO NSMutableArray.new
    _adjustingBuilderConfig_becameReadyInParent = becameReady . _env_frame <$> UIKitViewBuilderT ask
    _adjustingBuilderConfig_newHolder           = retainObj =<< uikitViewBuilder_newHolderView
    _adjustingBuilderConfig_appendHolder        = appendFragment
    _adjustingBuilderConfig_newMarker           = retainObj =<< uikitViewBuilder_newMarkerView
    _adjustingBuilderConfig_appendMarker        = append
    _adjustingBuilderConfig_removeMarker        = UIView.removeFromSuperview

    _adjustingBuilderConfig_runChild _env_parentView _buildFrame_retainObjs _buildFrame_commitAction child = do
      _buildFrame_unreadyChildren <- liftIO $ newIORef 0
      let _env_frame = BuildFrame {..}
          childEnv = Env {..}
      result <- runReaderT (unUIKitViewBuilderT child) childEnv
      (result,) . (> 0) <$> liftIO (readIORef _buildFrame_unreadyChildren)

    _adjustingBuilderConfig_collectViewsBetween startObj endObj =
      withObjPtr startObj $ \ start ->
      withObjPtr endObj $ \ end ->
        retainObj =<< uikitViewBuilder_collectViewsBetween start end
    _adjustingBuilderConfig_deleteViewsBetween startObj endObj =
      withObjPtr startObj $ \ start ->
      withObjPtr endObj $ \ end ->
        uikitViewBuilder_deleteViewsBetween start end
    _adjustingBuilderConfig_replaceBetweenMarkersWithHolder startObj endObj holderObj =
      withObjPtr startObj $ \ start ->
      withObjPtr endObj $ \ end ->
      withObjPtr holderObj $ \ holder ->
        uikitViewBuilder_replaceBetweenMarkersWithHolder start end holder
    _adjustingBuilderConfig_insertMarkerBeforeMarker markerObj beforeObj =
      withObjPtr markerObj $ \ marker ->
      withObjPtr beforeObj $ \ before ->
        uikitViewBuilder_insertMarkerBeforeMarker marker before

-- |'AdjustingBuilder.runWithReplaceImpl' using 'uikitViewBuilderConfig'. Exposed as a separate name for specialization.
{-# INLINABLE uikitRunWithReplace #-}
uikitRunWithReplace
  :: forall t m a b. SupportsUIKitViewBuilder t m
  => UIKitViewBuilderT t m a -> Event t (UIKitViewBuilderT t m b)
  -> UIKitViewBuilderT t m (a, Event t b)
uikitRunWithReplace =
  AdjustingBuilder.runWithReplaceImpl uikitViewBuilderConfig

-- |'AdjustingBuilder.traverseIntMapWithKeyWithAdjustImpl' using 'uikitViewBuilderConfig'. Exposed as a separate name for specialization.
{-# INLINABLE uikitTraverseIntMapWithKeyWithAdjust #-}
uikitTraverseIntMapWithKeyWithAdjust
  :: forall t m (v :: *) (v' :: *). SupportsUIKitViewBuilder t m
  => (IntMap.Key -> v -> UIKitViewBuilderT t m v')
  -> IntMap v -> Event t (PatchIntMap v)
  -> UIKitViewBuilderT t m (IntMap v', Event t (PatchIntMap v'))
uikitTraverseIntMapWithKeyWithAdjust =
  AdjustingBuilder.traverseIntMapWithKeyWithAdjustImpl uikitViewBuilderConfig

-- |'AdjustingBuilder.traverseDMapWithKeyWithAdjustImpl' using 'uikitViewBuilderConfig'. Exposed as a separate name for specialization.
{-# INLINABLE uikitTraverseDMapWithKeyWithAdjust #-}
uikitTraverseDMapWithKeyWithAdjust
  :: forall t m (k :: * -> *) (v :: * -> *) (v' :: * -> *). (SupportsUIKitViewBuilder t m, GCompare k)
  => (forall a. k a -> v a -> UIKitViewBuilderT t m (v' a))
  -> DMap k v -> Event t (PatchDMap k v)
  -> UIKitViewBuilderT t m (DMap k v', Event t (PatchDMap k v'))
uikitTraverseDMapWithKeyWithAdjust =
  AdjustingBuilder.traverseDMapWithKeyWithAdjustImpl uikitViewBuilderConfig

-- |'AdjustingBuilder.traverseDMapWithKeyWithAdjustWithmoveImpl' using 'uikitViewBuilderConfig'. Exposed as a separate name for specialization.
{-# INLINABLE uikitTraverseDMapWithKeyWithAdjustWithMove #-}
uikitTraverseDMapWithKeyWithAdjustWithMove
  :: forall t m (k :: * -> *) (v :: * -> *) (v' :: * -> *). (SupportsUIKitViewBuilder t m, GCompare k)
  => (forall a. k a -> v a -> UIKitViewBuilderT t m (v' a))
  -> DMap k v -> Event t (PatchDMapWithMove k v)
  -> UIKitViewBuilderT t m (DMap k v', Event t (PatchDMapWithMove k v'))
uikitTraverseDMapWithKeyWithAdjustWithMove =
  AdjustingBuilder.traverseDMapWithKeyWithAdjustWithMoveImpl uikitViewBuilderConfig

-- |Run a 'MainThread' immediately in a 'UIKitViewBuilderT', which should be safe because the top level builder should run on the main thread as well as
-- requested actions using the 'Requester'.
liftMainThread :: MonadIO m => MainThread a -> UIKitViewBuilderT t m a
liftMainThread = liftIO . unsafeUnMainThread -- not unsafe because view builder actions are always running on the main thread or requested.

-- |Run a 'UIKitViewBuilderT' which uses some channel for trigger events and adds views to a given parent.
{-# INLINABLE runUIKitViewBuilderT #-}
runUIKitViewBuilderT
  :: ( MonadFix m
     , MonadRef m, Ref m ~ IORef
     , MonadReflexCreateTrigger t m
     , MonadIO m
     , PerformEvent t m, MonadIO (Performable m)
     , SafeObjCoerce view UIViewType
     )
  => UIKitViewBuilderT t m a
  -- ^The build action to run.
  -> ObjPtr view
  -- ^The parent view to build views into, typically a new empty root @UIView@.
  -> Chan [DSum (EventTriggerRef t) TriggerInvocation]
  -- ^The channel where asynchronous event triggers will be enqueued by the builder, usually handled by some asynchronous event firing thread which runs host
  -- frames in response to batches of triggers.
  -> m (a, Env)
runUIKitViewBuilderT (UIKitViewBuilderT ma) view eventChan = do
  liftIO checkMainThread
  let _buildFrame_commitAction = pure ()
  _buildFrame_retainObjs <- liftIO NSMutableArray.new
  _buildFrame_unreadyChildren <- liftIO $ newIORef 0
  let _env_frame = BuildFrame {..}
      _env_parentView = asUIView view
      env = Env {..}

  flip runTriggerEventT eventChan $ do
    rec
      (a, requests) <- runRequesterT (runReaderT ma env) responses
      responses <- performEventAsync $ ffor requests $ \ requestDMap callback ->
        liftIO . dispatchAsyncMain $ void . liftIO . callback =<< traverseRequesterData (fmap Identity) requestDMap

    pure (a, env)

-- |Add an object to the '_buildFrame_retainObjs' for the current 'BuildFrame', making sure it stays alive as long as the views of this build do.
{-# INLINE appendRetainObj #-}
appendRetainObj :: (SafeObjCoerce obj ObjType, MonadIO m) => ObjPtr obj -> UIKitViewBuilderT t m ()
appendRetainObj obj = do
  Env { _env_frame = BuildFrame {..} } <- UIKitViewBuilderT ask
  liftIO $ NSMutableArray.addObject _buildFrame_retainObjs obj

-- |Append a new subview to the current parent view.
{-# INLINE append #-}
append :: (SafeObjCoerce view ObjType, SafeObjCoerce view UIViewType, MonadIO m) => ObjPtr view -> UIKitViewBuilderT t m ()
append view = do
  Env {..} <- UIKitViewBuilderT ask
  liftMainThread $ UIView.addSubview _env_parentView view

-- |Raw FFI binding to add subviews of a holder to a parent.
foreign import ccall uikitViewBuilder_addSubviewsFromHolder :: Ptr UIViewType -> Ptr UIViewType -> MainThread ()
-- |Append all views in a holder as subviews to the current parent view.
{-# INLINE appendFragment #-}
appendFragment :: MonadIO m => UIView -> UIKitViewBuilderT t m ()
appendFragment holderObj = do
  Env {..} <- UIKitViewBuilderT ask
  liftMainThread $
    withObjPtr _env_parentView $ \ parent ->
    withObjPtr holderObj $ \ holder ->
      uikitViewBuilder_addSubviewsFromHolder parent holder

-- |Add a gesture recognizer to a view while making sure its associated recognizer target stays alive in the current 'BuildFrame', returning an @Event@ which
-- triggers each time the recognition state changes.
addGestureRecognizer
  :: (SafeObjCoerce view UIViewType, SafeObjCoerce recognizer UIGestureRecognizerType, SupportsUIKitViewBuilder t m)
  => ObjPtr view
  -- ^The view to add a gesture recognizer to.
  -> (Obj -> IO (ObjPtr recognizer))
  -- ^Constructor for the gesture recognizer, taking the target object to trigger (with selector @handler:@).
  -> (ObjPtr recognizer -> IO a)
  -- ^Action to read out relevant state information from the recognizer when producing the output event.
  -> UIKitViewBuilderT t m (Event t (GestureState a))
addGestureRecognizer view mkRecognizer readRecognizerData = do
  (ev, trigger) <- newTriggerEvent
  let readRecognizer r =
        UIGestureRecognizer.getState r >>= \ case
          UIGestureRecognizerState_Possible   -> pure GestureState_None
          UIGestureRecognizerState_Began      -> GestureState_Began   <$> readRecognizerData (downCastObj r)
          UIGestureRecognizerState_Changed    -> GestureState_Changed <$> readRecognizerData (downCastObj r)
          UIGestureRecognizerState_Ended      -> GestureState_Ended   <$> readRecognizerData (downCastObj r)
          UIGestureRecognizerState_Cancelled  -> GestureState_Ended   <$> readRecognizerData (downCastObj r)
          UIGestureRecognizerState_Failed     -> pure GestureState_None
      callback = trigger <=< readRecognizer
  targetObj <- liftIO . GestureRecognizerTarget.new $ liftIO . callback
  appendRetainObj targetObj
  recognizer <- liftIO $ mkRecognizer (asObj targetObj)
  liftIO $ GestureRecognizerTarget.setRecognizer targetObj recognizer
  liftMainThread $ UIView.addGestureRecognizer view recognizer
  pure ev

-- |Read the values from the current state of a 'UIPanGestureRecognizer' to make a 'PanGesture' record. Typically used with 'addGestureRecognizer'.
readPanGestureRecognizer :: UIPanGestureRecognizer -> IO PanGesture
readPanGestureRecognizer pr =
  PanGesture
    <$> (pointFromCGPoint <$> UIPanGestureRecognizer.getTranslationInSuperview pr)
    <*> (pointFromCGPoint <$> UIPanGestureRecognizer.getVelocityInSuperview pr)
