{-# LANGUAGE RecordWildCards #-}
-- |Generic @UIViewController@ subclass that is configured with callbacks for the lifecycle events of a view controller.
module UIKit.Generic.ViewController
  (
  -- * Configuring
    ViewControllerConfig(..), defaultViewControllerConfig
  -- * Creating
  , new
  -- * Raw FFI bindings
  , genericViewController_new
  ) where

import Control.Applicative ((<$>), pure)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad ((=<<))
import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr (Ptr)
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr, freeStablePtr)
import ObjC (hsobjc_retain, retainObj, withObjPtr)
import qualified UIKit.Generic.View as GenericView
import UIKit.Types (MainThread(..), UIView, UIViewType, UIViewController, UIViewControllerType)


-- |Configuration for a generic view controller with a user-specified @a@ to hold as long as the view controller remains live and the callbacks to invoke when
-- view controller lifecycle events occur.
--
-- __Warning:__ take care to not refer back to the @UIViewController@ strongly from @a@, as that will cause a reference cycle which will never be deallocated.
data ViewControllerConfig a = ViewControllerConfig
  { _viewController_a :: a
  -- ^User data to keep while the view controller is live and is passed to each callback invocation.
  --
  -- __Warning:__ take care to not refer back to the @UIViewController@ strongly from @a@, as that will cause a reference cycle which will never be deallocated.
  , _viewController_loadView :: a -> MainThread UIView
  -- ^Callback to invoke when @UIViewController - loadView@ is called which builds and returns the initial view for the view controller.
  , _viewController_viewDidLoad :: a -> MainThread ()
  -- ^Callback to invoke when @UIViewController - viewDidLoad@ is called.
  , _viewController_didReceiveMemoryWarning :: a -> MainThread ()
  -- ^Callback to invoke when @UIViewController - didReceiveMemoryWarning@ is called.
  }

-- |Create a 'ViewControllerConfig' with the given value and no-op callbacks. '_viewController_loadView' returns an empty "UIKit.Generic.View".
defaultViewControllerConfig :: a -> ViewControllerConfig a
defaultViewControllerConfig a = ViewControllerConfig
  { _viewController_a                       = a
  , _viewController_loadView                = \ _ -> fst <$> GenericView.new (\ _ -> pure $ GenericView.defaultViewConfig ())
  , _viewController_viewDidLoad             = \ _ -> pure ()
  , _viewController_didReceiveMemoryWarning = \ _ -> pure ()
  }

-- |Raw FFI binding to @genericViewController_new@ which takes a callback to run during initialization.
foreign import ccall genericViewController_new
  :: StablePtr (UIViewController -> MainThread (StablePtr (ViewControllerConfig a)))
  -> MainThread (Ptr UIViewControllerType)

-- |Create a new generic view controller, running the given callback after the view controller has been allocated but before it's initialized which creates the
-- configuration for the new view controller.
new
  :: (UIViewController -> MainThread (ViewControllerConfig a))
  -> MainThread (UIViewController, ViewControllerConfig a)
new initialize = do
  configRef <- liftIO newEmptyMVar
  callbackPtr <- liftIO . newStablePtr $ \ vc -> do
    config <- initialize vc
    liftIO $ putMVar configRef config
    liftIO $ newStablePtr config
  vc <- retainObj =<< genericViewController_new callbackPtr
  liftIO $ freeStablePtr callbackPtr
  config <- liftIO $ takeMVar configRef
  pure (vc, config)

foreign export ccall genericViewController_initialize
  :: Ptr UIViewControllerType
  -> StablePtr (UIViewController -> MainThread (StablePtr (ViewControllerConfig a)))
  -> MainThread (StablePtr (ViewControllerConfig a))
-- |Callback from ObjC during initialization to invoke the initialization callback.
genericViewController_initialize
  :: Ptr UIViewControllerType
  -> StablePtr (UIViewController -> MainThread (StablePtr (ViewControllerConfig a)))
  -> MainThread (StablePtr (ViewControllerConfig a))
genericViewController_initialize vcPtr callbackPtr = do
  vc <- retainObj vcPtr
  callback <- liftIO $ deRefStablePtr callbackPtr
  callback vc

foreign export ccall genericViewController_release
  :: StablePtr (ViewControllerConfig a)
  -> MainThread ()
-- |Callback from ObjC when the view controller is released to release the configuration 'StablePtr'
genericViewController_release
  :: StablePtr (ViewControllerConfig a)
  -> MainThread ()
genericViewController_release = liftIO . freeStablePtr

foreign export ccall genericViewController_loadView
  :: StablePtr (ViewControllerConfig a)
  -> MainThread (Ptr UIViewType)
-- |Callback from ObjC to trigger the '_viewController_loadView' callback.
genericViewController_loadView
  :: StablePtr (ViewControllerConfig a)
  -> MainThread (Ptr UIViewType)
genericViewController_loadView configPtr = do
  ViewControllerConfig {..} <- liftIO $ deRefStablePtr configPtr
  vo <- _viewController_loadView _viewController_a
  withObjPtr vo $ \ v -> do
    liftIO $ hsobjc_retain v
    pure v

foreign export ccall genericViewController_viewDidLoad
  :: StablePtr (ViewControllerConfig a)
  -> MainThread ()
-- |Callback from ObjC to trigger the '_viewController_viewDidLoad' callback.
genericViewController_viewDidLoad
  :: StablePtr (ViewControllerConfig a)
  -> MainThread ()
genericViewController_viewDidLoad configPtr = do
  ViewControllerConfig {..} <- liftIO $ deRefStablePtr configPtr
  _viewController_viewDidLoad _viewController_a

foreign export ccall genericViewController_didReceiveMemoryWarning
  :: StablePtr (ViewControllerConfig a)
  -> MainThread ()
-- |Callback from ObjC to trigger the '_viewController_didReceiveMemoryWarning' callback.
genericViewController_didReceiveMemoryWarning
  :: StablePtr (ViewControllerConfig a)
  -> MainThread ()
genericViewController_didReceiveMemoryWarning configPtr = do
  ViewControllerConfig {..} <- liftIO $ deRefStablePtr configPtr
  _viewController_didReceiveMemoryWarning _viewController_a


