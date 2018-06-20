{-# LANGUAGE FlexibleContexts #-}
-- |Class and instance methods of the @UIViewController@ class.
module UIKit.UIViewController
  (
  -- * Instance methods
    setView
  -- * Raw FFI bindings
  , uiViewController_setView
  ) where

import Foreign.Ptr (Ptr)
import ObjC (ObjPtr, SafeObjCoerce, withObjPtr)
import UIKit.Types (MainThread(..), UIViewType, UIViewControllerType, asUIView, asUIViewController)


-- |Raw FFI binding to @UIViewController - setView:@
foreign import ccall uiViewController_setView :: Ptr UIViewControllerType -> Ptr UIViewType -> MainThread ()
-- |@UIViewController - setView:@ - set the root view of the view controller.
setView :: (SafeObjCoerce viewController UIViewControllerType, SafeObjCoerce view UIViewType) => ObjPtr viewController -> ObjPtr view -> MainThread ()
setView vco vo =
  withObjPtr (asUIViewController vco) $ \ vc ->
  withObjPtr (asUIView vo) $ \ v ->
    uiViewController_setView vc v

