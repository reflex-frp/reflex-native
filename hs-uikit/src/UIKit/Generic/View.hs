{-# LANGUAGE RecordWildCards #-}
-- |A generic subclass of 'UIView' where lifecycle callbacks are implemented by calling back to Haskell actions.
module UIKit.Generic.View
  (
  -- * Configuring
    ViewConfig(..), defaultViewConfig
  -- * Creating
  , new
  -- * Raw FFI bindings
  , genericView_new
  ) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad ((=<<))
import Control.Monad.IO.Class (liftIO)
import CoreGraphics (CGRect)
import Foreign.Ptr (Ptr)
import Foreign.StablePtr (StablePtr, newStablePtr, freeStablePtr, deRefStablePtr)
import Foreign.Storable (peek)
import ObjC (retainObj)
import UIKit.Types (MainThread(..), UIView, UIViewType)


-- |Configuration for a generic view including some user-specified data @a@ which will be held as long as the view remains live and actions to execute during
-- the view lifecycle.
--
-- __Warning:__ take care to not refer back to the @UIView@ strongly from @a@, as that will cause a reference cycle which will never be deallocated.
data ViewConfig a = ViewConfig
  {
    _view_a :: a
  -- ^ User-defined data to keep alive with the view and pass to each callback.
  -- __Warning:__ take care to not refer back to the @UIView@ strongly from @a@, as that will cause a reference cycle which will never be deallocated.
  , _view_drawRect :: a -> CGRect -> MainThread ()
  -- ^ Callback to invoke when @UIView - drawRect:@ is called on the view.
  }

-- |Create a new 'ViewConfig' with default no-op callbacks and the given @a@.
defaultViewConfig :: a -> ViewConfig a
defaultViewConfig a = ViewConfig
  { _view_a        = a
  , _view_drawRect = \ _ _ -> pure ()
  }

-- |Raw FFI binding to @[[GenericView alloc] initWithCallback:]@.
foreign import ccall genericView_new
  :: StablePtr (UIView -> MainThread (StablePtr (ViewConfig a)))
  -> MainThread (Ptr UIViewType)

-- |Create a new generic view, invoking the given callback after the view is allocated but before it finishes initializing to configure the view.
new
  :: (UIView -> MainThread (ViewConfig a))
  -> MainThread (UIView, ViewConfig a)
new initialize = do
  configRef <- liftIO $ newEmptyMVar
  callbackPtr <- liftIO $ newStablePtr $ \ v -> do
    config <- initialize v
    liftIO $ putMVar configRef config
    liftIO $ newStablePtr config
  v <- retainObj =<< genericView_new callbackPtr
  liftIO $ freeStablePtr callbackPtr
  config <- liftIO $ takeMVar configRef
  pure (v, config)

foreign export ccall genericViewImpl_initialize
  :: Ptr UIViewType
  -> StablePtr (UIView -> MainThread (StablePtr (ViewConfig a)))
  -> MainThread (StablePtr (ViewConfig a))
-- |Callback from the ObjC code to invoke the Haskell initialization callback.
genericViewImpl_initialize
  :: Ptr UIViewType
  -> StablePtr (UIView -> MainThread (StablePtr (ViewConfig a)))
  -> MainThread (StablePtr (ViewConfig a))
genericViewImpl_initialize vPtr callbackPtr = do
  v <- retainObj vPtr
  callback <- liftIO $ deRefStablePtr callbackPtr
  callback v

foreign export ccall genericViewImpl_release
  :: StablePtr (ViewConfig a)
  -> MainThread ()
-- |Callback from the ObjC code when the view is being released to release the 'StablePtr' holding the configuration live.
genericViewImpl_release
  :: StablePtr (ViewConfig a)
  -> MainThread ()
genericViewImpl_release = liftIO . freeStablePtr

foreign export ccall genericViewImpl_drawRect
  :: StablePtr (ViewConfig a)
  -> Ptr CGRect
  -> MainThread ()
-- |Callback from the ObjC code when @UIView - drawRect:@ is called to draw the contents of the view by invoking the configured '_view_drawRect' callback.
genericViewImpl_drawRect
  :: StablePtr (ViewConfig a)
  -> Ptr CGRect
  -> MainThread ()
genericViewImpl_drawRect configPtr rectPtr = do
  ViewConfig {..} <- liftIO $ deRefStablePtr configPtr
  rect <- liftIO $ peek rectPtr
  _view_drawRect _view_a rect

