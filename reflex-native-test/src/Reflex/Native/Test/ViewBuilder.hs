{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |Simulated view hierarchy and associated 'ViewBuilder' and 'Adjustable' instances for running cross-platform components in a headless test environment, and
-- testing "Reflex.Native.AdjustingBuilder".
module Reflex.Native.Test.ViewBuilder
  (
  -- * @TestViewBuilderT@
    SupportsTestViewBuilder, TestViewBuilderT(..), TestViewBuilderSpace, runTestViewBuilderT
  -- * 'AdjustingBuilderConfig' and associated hierarchy manipulation primitives
  , testViewBuilderConfig, append, reparentView, reparentViews, replaceBetweenWith
  -- * Implementation details
  , TestRequesterT, BuildFrame(..), Env(..)
  ) where

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, newTVar, readTVar, readTVarIO, writeTVar, modifyTVar', swapTVar)
import Control.Monad (Monad, (=<<), (<=<), when)
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Exception (MonadAsyncException, MonadException)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad(type PrimState, primitive))
import Control.Monad.Ref (MonadAtomicRef(atomicModifyRef), MonadRef(type Ref, newRef, readRef, writeRef, modifyRef, modifyRef'))
import Control.Monad.Reader (ReaderT(..), ask, asks)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Dependent.Sum (DSum)
import Data.Foldable (for_, traverse_)
import Data.Functor (Functor(fmap), (<$), void)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(Pair))
import Data.IORef (IORef)
import Data.Monoid ((<>))
import Data.Sequence (Seq, ViewL((:<)), (|>), (<|), (><))
import qualified Data.Sequence as Seq
import Data.Text (Text, unpack)
import qualified Rank2
import Reflex
  ( Adjustable(runWithReplace, traverseDMapWithKeyWithAdjust, traverseIntMapWithKeyWithAdjust, traverseDMapWithKeyWithAdjustWithMove)
  , never
  , MonadHold(hold, holdDyn, holdIncremental, buildDynamic, headE)
  , MonadSample(sample)
  , Reflex
  , ffor, headE
  )
import Reflex.Host.Class (MonadReflexCreateTrigger(newEventWithTrigger, newFanEventWithTrigger))
import Reflex.Native.AdjustingBuilder (AdjustingBuilderConfig(..))
import qualified Reflex.Native.AdjustingBuilder as AdjustingBuilder
import Reflex.Native.Gesture (GestureSpec(..))
import Reflex.Native.Test.Types
  ( TestHolder, TestView(..), TestViewCommon(..), TestTextView(..), TestContainerView(..), TestMarker(..)
  , newTestIdentity, _testView_common, tshowTestMarkerIdentity
  )
import Reflex.Native.TextConfig (TextConfig(..))
import Reflex.Native.ViewBuilder.Class
  ( ViewBuilder(type ViewBuilderSpace, buildTextView, buildView, placeRawView, wrapRawView, recognizeGesture)
  , ViewSpace(type RawTextView, type RawView), TextView(..), View(..)
  )
import Reflex.Native.ViewConfig (RawViewConfig(..), ViewConfig(..))
import Reflex.NotReady.Class (NotReady(notReady, notReadyUntil))
import Reflex.PerformEvent.Class (PerformEvent(type Performable, performEvent, performEvent_), performEventAsync)
import Reflex.PostBuild.Class (PostBuild(getPostBuild))
import Reflex.Requester.Base (RequesterT(..), runRequesterT, traverseRequesterData)
import Reflex.Requester.Class (Requester(type Request, type Response, requesting, requesting_))
import Reflex.TriggerEvent.Base (EventTriggerRef, TriggerEventT(..), runTriggerEventT, TriggerInvocation)
import Reflex.TriggerEvent.Class (TriggerEvent(newTriggerEvent, newTriggerEventWithOnComplete, newEventWithLazyTriggerWithOnComplete))


-- |Structure maintaining state for a frame on the nominal stack of builders. Each time a nested build that might be discarded occurs a new frame is "pushed"
-- by invoking a child builder action with a new @BuildFrame@, such as during @runWithReplace@ or other 'Adjustable' methods.
data BuildFrame = BuildFrame
  { _buildFrame_unreadyChildren :: TVar Int
  -- ^Number of children in the current frame which have indicated 'notReady' (or 'notReadyUntil') and which might prevent this frame and/or its parent frame(s)
  -- from being installed. An @Int@ for testing to keep track of over-readies.
  , _buildFrame_commitAction :: IO ()
  -- ^The action to execute when @_buildFrame_unreadyChildren@ goes to 0 after being nonzero which installs the frame's built hierarchy.
  , _buildFrame_hasCommitted :: TVar Bool
  -- ^TVar keeping track of whether this frame has been committed already or not, to trigger an error if it's committed more than once.
  }

-- |The environment a 'TestViewBuilderT' runs in, referring to a 'BuildFrame' for the current adjustable frame and the parent view to append new views to.
--
-- The parent view varies when building subviews ala 'buildView' but the build frame does not, while conversely the build frame varies but the parent view
-- might not when performing 'Adjustable' methods or similar.
data Env = Env
  { _env_views :: TestHolder
  -- ^The current holder to append views to
  , _env_frame :: BuildFrame
  -- ^The current dynamic ('Adjustable') frame that the builder is running in
  }

-- |Constraints required of a monad to support a 'TestViewBuilderT'.
type SupportsTestViewBuilder t m =
  ( Reflex t
  , MonadIO m
  , MonadHold t m
  , MonadFix m
  , MonadReflexCreateTrigger t m
  , MonadRef m, Ref m ~ Ref IO
  , Adjustable t m
  , PrimMonad m
  )

-- |The monad underneath the @ReaderT@ in a 'TestViewBuilderT'.
type TestRequesterT t m = RequesterT t IO Identity (TriggerEventT t m)

-- |'ViewBuilder' monad for building test view hierarchies.
newtype TestViewBuilderT t m a = TestViewBuilderT
  { unTestViewBuilderT :: ReaderT Env (TestRequesterT t m) a
  }

deriving instance Functor m => Functor (TestViewBuilderT t m)
deriving instance Monad m => Applicative (TestViewBuilderT t m)
deriving instance Monad m => Monad (TestViewBuilderT t m)
deriving instance MonadFix m => MonadFix (TestViewBuilderT t m)
deriving instance MonadIO m => MonadIO (TestViewBuilderT t m)
deriving instance MonadException m => MonadException (TestViewBuilderT t m)
deriving instance MonadAsyncException m => MonadAsyncException (TestViewBuilderT t m)

-- |Pass through 'PrimMonad'.
instance PrimMonad m => PrimMonad (TestViewBuilderT t m) where
  type PrimState (TestViewBuilderT t m) = PrimState m
  primitive = lift . primitive

-- |Straightforward lift.
instance MonadTrans (TestViewBuilderT t) where
  lift = TestViewBuilderT . lift . lift . lift

-- |Pass through 'MonadAtomicRef'.
instance MonadAtomicRef m => MonadAtomicRef (TestViewBuilderT t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

-- |Pass through 'MonadHold'.
instance MonadHold t m => MonadHold t (TestViewBuilderT t m) where
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
instance MonadRef m => MonadRef (TestViewBuilderT t m) where
  type Ref (TestViewBuilderT t m) = Ref m
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
instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (TestViewBuilderT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

-- |Pass through 'MonadSample'.
instance MonadSample t m => MonadSample t (TestViewBuilderT t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

-- |Pass through 'PerformEvent'.
instance PerformEvent t m => PerformEvent t (TestViewBuilderT t m) where
  type Performable (TestViewBuilderT t m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ performEvent e

-- |Pass through 'PostBuild'.
instance PostBuild t m => PostBuild t (TestViewBuilderT t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift getPostBuild

-- |Pass through 'Requester'.
instance (Reflex t, Monad m) => Requester t (TestViewBuilderT t m) where
  type Request (TestViewBuilderT t m) = IO
  type Response (TestViewBuilderT t m) = Identity
  requesting = TestViewBuilderT . lift . requesting
  requesting_ = TestViewBuilderT . lift . requesting_

-- |Pass through 'TriggerEvent'.
instance (Monad m, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m) => TriggerEvent t (TestViewBuilderT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = TestViewBuilderT . lift . lift $ newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = TestViewBuilderT . lift . lift $ newTriggerEventWithOnComplete
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete f = TestViewBuilderT . lift . lift $ newEventWithLazyTriggerWithOnComplete f

-- |Report a fault discovered during builder processing. Right now this just 'fail's, since it's assumed that will go somewhere useful for unit
-- testing. Replace with something better later when the required semantics are understood.
reportFault :: MonadIO m => Text -> m ()
reportFault t = liftIO . fail . unpack $ "fault detected in test builder: " <> t

-- |Create a 'TestViewCommon' based on a 'ViewConfig' with the initial values and request that the TVars get updated whenever the view config modification
-- events fire.
makeTestViewCommon :: (MonadIO m, Requester t m, MonadIO (Request m)) => ViewConfig t -> m (TestViewCommon TVar)
makeTestViewCommon (ViewConfig {..}) = do
  _testViewCommon_identity <- newTestIdentity
  _testViewCommon_style <- Rank2.traverse (liftIO . newTVarIO . runIdentity) _viewConfig_initialStyle
  _testViewCommon_layout <- liftIO $ newTVarIO _viewConfig_initialLayout
  _testViewCommon_accessibilityLabel <- liftIO $ newTVarIO _viewConfig_initialAccessibilityLabel
  let tvc = TestViewCommon {..}
  for_ _viewConfig_modifyStyle $ \ vs' ->
    void $ Rank2.traverse
      (\ (Pair tv ev) -> fmap Const . requesting_ . ffor ev $ \ a -> liftIO . atomically . writeTVar tv $! a)
      (Rank2.liftA2 Pair _testViewCommon_style vs')
  for_ _viewConfig_setLayout $ requesting_ . fmap (liftIO . atomically . writeTVar _testViewCommon_layout)
  for_ _viewConfig_setAccessibilityLabel $ requesting_ . fmap (liftIO . atomically . writeTVar _testViewCommon_accessibilityLabel)
  pure tvc

-- |Type of 'ViewSpace' for testing with the test types as the raw types, e.g. @RawView ~ TestView@.
data TestViewBuilderSpace

-- |Instance for testing.
instance ViewSpace TestViewBuilderSpace where
  type RawTextView TestViewBuilderSpace = TestTextView TVar
  type RawView TestViewBuilderSpace = TestView TVar

-- |@ViewBuilder@ for testing.
instance SupportsTestViewBuilder t m => ViewBuilder t (TestViewBuilderT t m) where
  type ViewBuilderSpace (TestViewBuilderT t m) = TestViewBuilderSpace

  buildTextView (TextConfig {..}) = do
    _testTextView_common <- makeTestViewCommon _textConfig_viewConfig
    _testTextView_text <- liftIO . newTVarIO $ _textConfig_initialText
    _testTextView_style <- Rank2.traverse (liftIO . newTVarIO . runIdentity) _textConfig_initialStyle
    for_ _textConfig_modifyStyle $ \ ts' ->
      void $ Rank2.traverse
        (\ (Pair tv ev) -> fmap Const . requesting_ . ffor ev $ \ a -> liftIO . atomically . writeTVar tv $! a)
        (Rank2.liftA2 Pair _testTextView_style ts')

    let view = TestTextView {..}

    append $ TestView_Text view

    pure $ TextView view

  buildView cfg child = do
    _testContainerView_common <- makeTestViewCommon cfg
    _testContainerView_contents <- liftIO $ newTVarIO Seq.empty

    parentEnv <- TestViewBuilderT ask
    let childEnv = parentEnv { _env_views = _testContainerView_contents }
    result <- TestViewBuilderT . lift $ runReaderT (unTestViewBuilderT child) childEnv

    let view = TestView_Container $ TestContainerView {..}

    append view

    pure (result, View view)

  placeRawView = append

  wrapRawView view (RawViewConfig {..}) = do
    for_ (_testView_common view) $ \ (TestViewCommon {..}) -> do
      for_ _rawViewConfig_modifyStyle $ \ vs' ->
        void $ Rank2.traverse
          (\ (Pair tv ev) -> fmap Const . requesting_ . ffor ev $ \ a -> liftIO . atomically . writeTVar tv $! a)
          (Rank2.liftA2 Pair _testViewCommon_style vs')
      for_ _rawViewConfig_setLayout $ requesting_ . fmap (liftIO . atomically . writeTVar _testViewCommon_layout)
      for_ _rawViewConfig_setAccessibilityLabel $ requesting_ . fmap (liftIO . atomically . writeTVar _testViewCommon_accessibilityLabel)
    pure $ View view

  recognizeGesture (View _) = \ case
    GestureSpec_Pan -> pure never -- FIXME

-- |Keep track of the readiness state of view hierarchies when testing.
instance SupportsTestViewBuilder t m => NotReady t (TestViewBuilderT t m) where
  notReadyUntil trigger = do
    notReady
    first <- headE trigger
    env <- TestViewBuilderT ask
    requesting_ $ becameReadyIn (_env_frame env) <$ first
  notReady = do
    Env { _env_frame = BuildFrame {..} } <- TestViewBuilderT ask
    new <- liftIO . atomically $ do
      uc <- readTVar _buildFrame_unreadyChildren
      let uc' = succ uc
      writeTVar _buildFrame_unreadyChildren $! uc'
      pure uc'
    when (new == 0) . lift . reportFault $ "became not ready after being over-ready"

-- |Helper for decrementing the unready children counter of a build frame and possibly triggeirng the commit action.
becameReadyIn :: BuildFrame -> IO ()
becameReadyIn (BuildFrame {..}) = do
  new <- atomically $ do
    uc <- readTVar _buildFrame_unreadyChildren
    let uc' = pred uc
    writeTVar _buildFrame_unreadyChildren $! uc'
    pure uc'
  when (new <= 0) $ do
    wasCommitted <- atomically (swapTVar _buildFrame_hasCommitted True)
    if wasCommitted
      then reportFault "frame committed more than once"
      else _buildFrame_commitAction

-- |"Reflex.Native.AdjustingBuilder" configured for testing.
instance SupportsTestViewBuilder t m => Adjustable t (TestViewBuilderT t m) where
  runWithReplace = AdjustingBuilder.runWithReplaceImpl testViewBuilderConfig
  traverseIntMapWithKeyWithAdjust = AdjustingBuilder.traverseIntMapWithKeyWithAdjustImpl testViewBuilderConfig
  traverseDMapWithKeyWithAdjust = AdjustingBuilder.traverseDMapWithKeyWithAdjustImpl testViewBuilderConfig
  traverseDMapWithKeyWithAdjustWithMove = AdjustingBuilder.traverseDMapWithKeyWithAdjustWithMoveImpl testViewBuilderConfig

-- |'AdjustingBuilderConfig' for manipulating the test view hierarchy.
testViewBuilderConfig
  :: MonadIO m
  => AdjustingBuilderConfig (TestViewBuilderT t m) (TestRequesterT t m) IO TestMarker TestHolder ()
testViewBuilderConfig = AdjustingBuilderConfig {..}
  where
    _adjustingBuilderConfig_liftBase = liftIO
    _adjustingBuilderConfig_lift = TestViewBuilderT . lift
    _adjustingBuilderConfig_newSlotAddendum = pure ()
    _adjustingBuilderConfig_becameReadyInParent = becameReadyIn <$> TestViewBuilderT (asks _env_frame)
    _adjustingBuilderConfig_newHolder = liftIO $ newTVarIO Seq.empty
    _adjustingBuilderConfig_newMarker = liftIO $ TestMarker <$> newTestIdentity <*> newTVarIO Nothing

    _adjustingBuilderConfig_runChild _env_views () _buildFrame_commitAction child = do
      _buildFrame_unreadyChildren <- liftIO $ newTVarIO 0
      _buildFrame_hasCommitted <- liftIO $ newTVarIO False
      let _env_frame = BuildFrame {..}
          childEnv = Env {..}
      result <- runReaderT (unTestViewBuilderT child) childEnv
      (result,) . (== 0) <$> liftIO (readTVarIO _buildFrame_unreadyChildren)

    _adjustingBuilderConfig_appendHolder seqTv = do
      Env { _env_views } <- TestViewBuilderT ask
      liftIO . atomically $ do
        s <- readTVar seqTv
        reparentViews (Just _env_views) s
        modifyTVar' _env_views (<> s)

    _adjustingBuilderConfig_collectViewsBetween = replaceBetweenWith (pure mempty) "collectViewsBetween"
    _adjustingBuilderConfig_deleteViewsBetween s e = void $ replaceBetweenWith (pure mempty) "deleteViewsBetween" s e
    _adjustingBuilderConfig_replaceBetweenMarkersWithHolder s e h = void $ replaceBetweenWith (readTVar h) "replaceBetweenMarkersWithHolder" s e

    _adjustingBuilderConfig_appendMarker m = do
      Env { _env_views } <- TestViewBuilderT ask
      liftIO . atomically $ do
        let asView = TestView_Marker m
        reparentView (Just _env_views) asView
        modifyTVar' _env_views (|> asView)

    _adjustingBuilderConfig_insertMarkerBeforeMarker m before = do
      let faultPrefix = "insertMarkerBeforeMarker (" <> tshowTestMarkerIdentity m <> ") (" <> tshowTestMarkerIdentity before <> "): "
          mAsView = TestView_Marker m
          beforeAsView = TestView_Marker before
      either (\ t -> reportFault (faultPrefix <> t)) pure <=<
        atomically . runExceptT $ do
          parentTv <- maybe (throwError "reference marker has no parent") pure =<< (liftBase . readTVar . _testMarker_parent) before
          oldParentTvMay <- liftBase . readTVar . _testMarker_parent $ m
          for_ oldParentTvMay $ \ oldParentTv -> liftBase $ modifyTVar' oldParentTv (Seq.filter (/= mAsView))
          subviews <- liftBase $ readTVar parentTv
          case Seq.spanl (/= beforeAsView) subviews of
            (p, s)
              | Seq.null s -> throwError "reference marker not in parent's subviews"
              | otherwise  -> liftBase $ writeTVar parentTv $! (p |> mAsView) >< s
          liftBase $ reparentView (Just parentTv) mAsView

    _adjustingBuilderConfig_removeMarker m = do
      let faultPrefix = "removeMarker (" <> tshowTestMarkerIdentity m <> "): "
          mAsView = TestView_Marker m
      either (\ t -> reportFault (faultPrefix <> t)) pure <=<
        atomically . runExceptT $ do
          oldParentTvMay <- liftBase . readTVar . _testMarker_parent $ m
          for_ oldParentTvMay $ \ oldParentTv -> liftBase $ modifyTVar' oldParentTv (Seq.filter (/= mAsView))
          liftBase $ reparentView Nothing mAsView

-- |Append a 'TestView' to the current build parent.
append :: MonadIO m => TestView TVar -> TestViewBuilderT t m ()
append v = do
  Env { _env_views } <- TestViewBuilderT ask
  liftIO . atomically $ modifyTVar' _env_views (|> v)

-- |Move all views in the given sequence to a new parent by changing any parent pointers each has using 'reparentView'.
reparentViews :: Maybe TestHolder -> Seq (TestView TVar) -> STM ()
reparentViews newParent = traverse_ (reparentView newParent)

-- |Move a view to a new parent by changing its parent pointer, if it has one.
reparentView :: Maybe TestHolder -> TestView TVar -> STM ()
reparentView parent = \ case
  TestView_Marker (TestMarker {..}) ->
    writeTVar _testMarker_parent parent
  _ -> pure ()

-- |Function which implements '_adjustingBuilderConfig_collectViewsBetween', '_adjustingBuilderConfig_deleteViewsBetween', and
-- '_adjustingBuilderConfig_replaceBetweenMarkersWithHolder'. Finds two markers in their parent view, reporting faults if they don't have a parent or they're
-- different, and collects up the views in between them and replaces them with the result of the given action. Returns an empty holder in any error case, but
-- reports faults via @reportFault@ to stdout.
replaceBetweenWith :: STM (Seq (TestView TVar)) -> Text -> TestMarker -> TestMarker -> IO TestHolder
replaceBetweenWith viewAction function start end = do
  let faultPrefix = function <> " (" <> tshowTestMarkerIdentity start <> ") (" <> tshowTestMarkerIdentity end <> "): "
  either (\ t -> reportFault (faultPrefix <> t) *> newTVarIO Seq.empty) pure <=<
    atomically . runExceptT $ do
      startParentTv <- maybe (throwError $ "start marker has no parent") pure =<< (liftBase . readTVar . _testMarker_parent) start
      endParentTv   <- maybe (throwError $ "end marker has no parent") pure =<< (liftBase . readTVar . _testMarker_parent) end

      when (startParentTv /= endParentTv) . throwError $ "start marker and end marker do not share parent"

      subviews <- liftBase $ readTVar startParentTv

      (prefix, afterStart) <-
        let (p, r) = Seq.spanl (/= TestView_Marker start) subviews in
          case Seq.viewl r of
            _ :< r' -> pure (p, r')
            _       -> throwError $ "start marker not in parent"
      (views, suffix) <-
        let (m, r) = Seq.spanl (/= TestView_Marker end) afterStart in
          case Seq.viewl r of
            _ :< r' -> pure (m, r')
            _       -> throwError $ "end marker not in parent"

      holder <- liftBase $ newTVar views
      liftBase $ reparentViews (Just holder) views

      views' <- lift viewAction
      liftBase $ reparentViews (Just startParentTv) views'
      liftBase . writeTVar startParentTv $! (prefix |> TestView_Marker start) >< views' >< (TestView_Marker end <| suffix)

      pure holder

-- |Run a 'TestViewBuilderT' which uses some channel for trigger events and adds views to a given parent.
{-# INLINABLE runTestViewBuilderT #-}
runTestViewBuilderT
  :: ( MonadFix m
     , MonadRef m, Ref m ~ IORef
     , MonadReflexCreateTrigger t m
     , MonadIO m
     , PerformEvent t m, MonadIO (Performable m)
     )
  => TestViewBuilderT t m a
  -- ^The 'TestViewBuilderT' action to run.
  -> TestHolder
  -- ^Where the builder should add new views, typically an empty holder.
  -> IO ()
  -- ^The root frame commit action.
  -> Chan [DSum (EventTriggerRef t) TriggerInvocation]
  -- ^A channel where asynchronous event triggers will be enqueued by the builder. Usually processed synchronously in the test running code.
  -> m (a, Env)
runTestViewBuilderT (TestViewBuilderT ma) _env_views _buildFrame_commitAction eventChan = do
  _buildFrame_unreadyChildren <- liftIO $ newTVarIO 0
  _buildFrame_hasCommitted <- liftIO $ newTVarIO False
  let _env_frame = BuildFrame {..}
      env = Env {..}

  flip runTriggerEventT eventChan $ do
    rec
      (a, requests) <- runRequesterT (runReaderT ma env) responses
      responses <- performEventAsync $ ffor requests $ \ requestDMap callback ->
        liftIO $ void . liftIO . callback =<< traverseRequesterData (fmap Identity) requestDMap

    pure (a, env)
