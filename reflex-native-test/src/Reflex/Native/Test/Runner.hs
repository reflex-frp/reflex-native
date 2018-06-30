{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |Test harness for running cross-platform components in 'TestViewBuilder' with isolated Spider instance.
module Reflex.Native.Test.Runner
  ( TestWidget, withTestHost, testWith, processEventsAndRead, processEvents ) where

import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVarIO, readTVarIO, writeTVar)
import Control.Monad ((<=<), when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Strict (ask, put, runRWST)
import Control.Monad.Trans.Class (lift)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Dependent.Sum (DSum((:=>)))
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Functor.Identity (Identity(..))
import Data.IORef (readIORef)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (catMaybes, isJust, maybe)
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import Reflex.Class (MonadHold, MonadSample)
import Reflex.Host.Class (MonadReadEvent(readEvent), newEventWithTriggerRef, subscribeEvent)
import Reflex.Native.Test.Types (TestEnv(..), TestEvaluation(..), TestHolder, TestView, traverseTestView)
import Reflex.Native.Test.ViewBuilder (BuildFrame(..), Env(..), TestViewBuilderT, runTestViewBuilderT)
import Reflex.PerformEvent.Base (FireCommand(..), PerformEventT, hostPerformEventT)
import Reflex.PostBuild.Base (PostBuildT, runPostBuildT)
import Reflex.Spider.Internal (HasSpiderTimeline, SpiderHost, SpiderTimeline, SpiderTimelineEnv, runSpiderHostForTimeline, withSpiderTimeline)
import Reflex.TriggerEvent.Base (EventTriggerRef(..), TriggerInvocation(..))


-- |The type conforming to 'Reflex.Native.MonadNativeConstraints' that test widgets run as.
type TestWidget x = PostBuildT (SpiderTimeline x) (TestViewBuilderT (SpiderTimeline x) (PerformEventT (SpiderTimeline x) (SpiderHost x)))

-- |Helper function used by 'testWith' and 'processEventsAndRead' to read out the current state of the view hierarchy.
snapshotViewHierarchy :: MonadIO m => TestHolder -> m (Seq (TestView Identity))
snapshotViewHierarchy rootHolder =
  liftIO $ traverse (traverseTestView (\ f -> fmap Identity . f <=< readTVarIO)) =<< readTVarIO rootHolder

-- |Enter an environment where 'testWith' can be run by creating a local Spider timeline for the test to execute in.
--
-- Usually one or more 'testWith' invocations are run inside. Do not execute test concurrently within a single test host.
withTestHost
  :: (forall proxy t x. (t ~ SpiderTimeline x, HasSpiderTimeline x) => proxy t -> SpiderHost x a)
  -> IO a
withTestHost action =
  withSpiderTimeline $ \ (env :: SpiderTimelineEnv x) ->
    runSpiderHostForTimeline (action (Proxy @(SpiderTimeline x))) env

-- |Test a widget by running its first build and then performing a series of evaluation steps. Usually wrapped by 'withTestHost'.
testWith
  :: HasSpiderTimeline x
  => TestWidget x a
  -- ^The build to run.
  -> (a -> TestEvaluation x b)
  -- ^The evaluation program to execute after the build is complete.
  -> SpiderHost x b
testWith widget evaluation = do
  _testEnv_rootHolder <- liftIO $ newTVarIO Seq.empty
  _testEnv_rootReady <- liftIO $ newTVarIO False
  _testEnv_eventChan <- liftIO newChan
  (stepCompleteEvent, _testEnv_stepCompleteTriggerRef) <- newEventWithTriggerRef
  (postBuildEvent, postBuildTriggerMayRef) <- newEventWithTriggerRef

  _testEnv_stepCompleteEventHandle <- subscribeEvent stepCompleteEvent

  ((a, env), _testEnv_fireCommand) <- hostPerformEventT $ do
    runTestViewBuilderT (runPostBuildT widget postBuildEvent) _testEnv_rootHolder (atomically $ writeTVar _testEnv_rootReady True) _testEnv_eventChan
  unreadyChildren <- liftIO . readTVarIO . _buildFrame_unreadyChildren . _env_frame $ env
  when (unreadyChildren == 0) $
    liftIO . atomically . writeTVar _testEnv_rootReady $ True
  postBuildTriggerMay <- liftIO $ readIORef postBuildTriggerMayRef
  for_ postBuildTriggerMay $ \ trigger ->
    runFireCommand _testEnv_fireCommand [trigger :=> Identity ()] (pure ())

  (b, _, _) <- runRWST (unTestEvaluation (evaluation a)) (TestEnv {..}) =<< snapshotViewHierarchy _testEnv_rootHolder
  pure b

-- |Process any pending event triggers until no more are pending, then process a read phase to read out states of @Event@s, @Behavior@s, or @Dynamic@s.
--
-- __Note on read actions:__ The read action may be performed many times, for two reasons.
--
-- 1. Internally, 'PerformEventT' runs one or more event propagation passes and the precise
-- number depends on how many times an event propagation triggers a performed event action; each time an event triggers a perform event, event propagation
-- occurs again after action is performed, until no more perform event requests are made.
--
-- Because each of these event propagations happen in a frame, if you read out an @Event@ using 'MonadReadEvent', then unless your @Event@ fires on each of the
-- propagations it will be firing in only one of the returned results, so be careful to consider this when reading @Event@s.
--
-- 2. @processEventsAndRead@ will process multiple batches of events until a marker event is processed and it can't know how many batches have been enqueued
-- on the internal @Chan@ until that marker event shows up. Each of the batches is processed in a separate frame, like @PerformEventT@ from the previous point.
processEventsAndRead
  :: forall x a. HasSpiderTimeline x
  => (forall m. (MonadReadEvent (SpiderTimeline x) m, MonadHold (SpiderTimeline x) m, MonadSample (SpiderTimeline x) m) => m a)
  -- ^Action to execute during the read phase allowing readout of @Event@s, @Behavior@s, and @Dynamic@s.
  -> TestEvaluation x (NonEmpty a)
processEventsAndRead readAction = do
  TestEnv {..} <- TestEvaluation ask

  liftIO $ writeChan _testEnv_eventChan [EventTriggerRef _testEnv_stepCompleteTriggerRef :=> TriggerInvocation () (pure ())]

  let processBatches :: DList a -> SpiderHost x (DList a)
      processBatches prevReadResults = do
        eventRequests <- liftIO $ readChan _testEnv_eventChan

        requestMays <- liftIO . for eventRequests $ \ (EventTriggerRef requestMayRef :=> TriggerInvocation a _) -> do
          fmap (:=> Identity a) <$> readIORef requestMayRef
        (completes, readResults) <- fmap unzip $
          runFireCommand _testEnv_fireCommand (catMaybes requestMays) $
            ((,) <$> readEvent _testEnv_stepCompleteEventHandle <*> readAction)
        liftIO . for_ eventRequests $ \ (_ :=> TriggerInvocation _ cb) -> cb

        (if any isJust completes then pure else processBatches) (DList.append prevReadResults (DList.fromList readResults))

  readResultsList <- TestEvaluation . lift $ DList.toList <$> processBatches DList.empty
  readResultsNonEmpty <-
    maybe (fail "read results should have been nonempty, but runFireCommand ran the read phase zero times?") pure (nonEmpty readResultsList)

  TestEvaluation . put =<< snapshotViewHierarchy _testEnv_rootHolder

  pure readResultsNonEmpty

-- |Process any pending event triggers until no more are pending. A simpler version of 'processEventsAndRead' when you don't need to inspect the state of the
-- FRP network after each propagation.
processEvents :: forall x. HasSpiderTimeline x => TestEvaluation x ()
processEvents = void $ processEventsAndRead (pure ())
