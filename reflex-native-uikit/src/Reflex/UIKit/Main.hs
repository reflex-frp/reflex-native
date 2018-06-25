{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fspecialise-aggressively #-}
-- |Functions for starting a Reflex UIKit application, all the way from @UIApplicationMain@ up to your application's root Reflex Native builder.
module Reflex.UIKit.Main
  (
  -- * Running an application or view controller
    run, runViewController
  -- * Specific types in UIKit apps
  , UIKitHost, runUIKitHost, UIKitTimeline, UIKitWidget
  ) where

import Control.Applicative ((<$>), pure)
import Control.Concurrent (forkOS)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Control.Monad ((=<<), forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Ref (readRef)
import Data.Dependent.Sum (DSum((:=>)))
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Functor.Identity (Identity(..))
import Data.IORef (IORef, readIORef)
import Data.Maybe (Maybe, catMaybes)
import Data.Traversable (for)
import Foreign.StablePtr (newStablePtr)
import Reflex.UIKit.Specializations () -- for SPECIALIZATION pragmas
import Reflex.UIKit.ViewBuilder (UIKitViewBuilderT, runUIKitViewBuilderT, _buildFrame_retainObjs, _env_frame)
import Reflex.Host.Class (ReflexHost(type EventTrigger), newEventWithTriggerRef)
import Reflex.PerformEvent.Base (FireCommand(..), PerformEventT, hostPerformEventT)
import Reflex.PostBuild.Base (PostBuildT, runPostBuildT)
import Reflex.Spider (Global, Spider, SpiderHost, runSpiderHost)
import Reflex.TriggerEvent.Base (EventTriggerRef(..), TriggerInvocation(..))
import UIKit.Generic.AppDelegate (AppDelegateConfig(_appDelegate_didFinishLaunchingWithOptions), defaultAppDelegateConfig, runGenericApplication)
import UIKit.Generic.View (defaultViewConfig)
import UIKit.Generic.ViewController (defaultViewControllerConfig)
import qualified UIKit.Generic.View as View
import qualified UIKit.Generic.ViewController as ViewController
import UIKit.Types (MainThread(..), UIView, UIViewController, unsafeInMainThread)
import UIKit.UIColor (colorWithRedGreenBlueAlpha)
import qualified UIKit.UIView as UIView
import qualified UIKit.UIViewController as UIViewController
import qualified UIKit.UIWindow as UIWindow


-- |The underlying Reflex host monad used for UIKit applications, @SpiderHost@.
type UIKitHost = SpiderHost Global

-- |The Reflex timeline in use for UIKit applications (@t@), @Spider@.
type UIKitTimeline = Spider

-- |The concrete type conforming to 'Reflex.Native.MonadNativeConstraints' that UIKit apps run in.
type UIKitWidget = PostBuildT UIKitTimeline (UIKitViewBuilderT UIKitTimeline (PerformEventT UIKitTimeline UIKitHost))

-- |How to run the underlying Reflex host monad 'UIKitHost' as an 'IO' action ('runSpiderHost').
runUIKitHost :: UIKitHost a -> IO a
runUIKitHost = runSpiderHost

-- |Run a @UIApplication@ using a generic app delegate which uses the given builder function to create and run the root view controller.
{-# INLINABLE run #-}
run :: (UIView -> UIKitWidget ()) -> IO ()
run mkRootWidget = runGenericApplication config
  where
    config = (defaultAppDelegateConfig ())
      { _appDelegate_didFinishLaunchingWithOptions }
    _appDelegate_didFinishLaunchingWithOptions _ _app _launchOptions window = do
      viewController <- runViewController (\ _ -> mkRootWidget)
      UIWindow.setRootViewController window viewController
      UIWindow.makeKeyAndVisible window
      pure True

-- |Run a view controller using the given builder to create and run the root view hierarchy.
--
-- __Warning:__ This function presently leaks an asynchronous message processing thread so should not be used for short lived controllers.
{-# INLINABLE runViewController #-}
runViewController :: (forall x. UIViewController -> UIView -> UIKitWidget x ()) -> MainThread UIViewController
runViewController mkWidget = fst <$> ViewController.new initializeController
  where
    initializeController vc = do
      (rootView, _) <- View.new $ \ v -> do
        UIView.setAutoresizesSubviews v False
        UIView.setAutoresizingMask v 0
        pure (defaultViewConfig ())

      unsafeInMainThread $ UIView.setBackgroundColor rootView =<< liftIO (colorWithRedGreenBlueAlpha 1.0 0.5 0.0 1.0)
      UIViewController.setView vc rootView

      let widget = mkWidget vc rootView

      ((env, events), fireCommand) <- liftIO . attachWidget'' $ \ events -> do
        (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
        (_, env) <- runUIKitViewBuilderT (runPostBuildT widget postBuild) rootView events
        pure ((env, events), postBuildTriggerRef)

      -- FIXME hold on to the root retain objs forever. any objects retained by a dynamic component of the initial build will not be in the root retain objs
      -- but instead held by the Requester, and we don't support end of lifetime for these view controllers anyway by virtue of processAsyncEvents running
      -- forever
      void . liftIO . newStablePtr . _buildFrame_retainObjs . _env_frame $ env

      liftIO $ processAsyncEvents events fireCommand -- FIXME runs forever, even if the view controller is only used for a short time
      pure $ defaultViewControllerConfig ()

-- |Type of channel used to pass event firings from the build or asynchronous activities resulting from the build such as native event handlers to the
-- asynchronous event loop implemented by 'processAsyncEvents'.
type EventChannel = Chan [DSum (EventTriggerRef UIKitTimeline) TriggerInvocation]

-- |Set up a fully working builder support environment with event processing and then invoke the action with the 'EventChannel'.
{-# INLINABLE attachWidget'' #-}
attachWidget''
  :: (EventChannel -> PerformEventT UIKitTimeline UIKitHost (a, IORef (Maybe (EventTrigger UIKitTimeline ()))))
  -> IO (a, FireCommand UIKitTimeline UIKitHost)
attachWidget'' f = do
  events <- newChan
  runUIKitHost $ do
    ((result, postBuildTriggerRef), fireCommand@(FireCommand fire)) <- hostPerformEventT $ f events
    postBuildTriggerMay <- readRef postBuildTriggerRef
    for_ postBuildTriggerMay $ \ postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] (pure ())
    pure (result, fireCommand)

-- |Loop forever waiting for events to show up on the given 'EventChannel' and process the firings back in the main thread.
processAsyncEvents :: EventChannel -> FireCommand UIKitTimeline UIKitHost -> IO ()
processAsyncEvents events (FireCommand fire) =
  void . forkOS . forever $ do
    eventRequests <- readChan events
    void . runUIKitHost $ do
      requestMays <- liftIO . for eventRequests $ \ (EventTriggerRef requestMayRef :=> TriggerInvocation a _) -> do
        requestMay <- readIORef requestMayRef
        pure $ (:=> Identity a) <$> requestMay
      _ <- fire (catMaybes requestMays) (pure ())
      liftIO . for_ eventRequests $ \ (_ :=> TriggerInvocation _ cb) -> cb


