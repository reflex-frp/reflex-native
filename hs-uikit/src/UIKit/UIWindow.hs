module UIKit.UIWindow where

import Control.Monad ((=<<))
import Foreign.C.Types (CChar(..))
import Foreign.Ptr (Ptr)
import ObjC (retainObj, withObjPtr, unObjcBool)
import UIKit.Types (MainThread(..), UIViewController, UIViewControllerType, UIWindow, UIWindowType)


foreign import ccall unsafe uiWindow_getRootViewController :: Ptr UIWindowType -> MainThread (Ptr UIViewControllerType)
getRootViewController :: UIWindow -> MainThread UIViewController
getRootViewController wo =
  withObjPtr wo $ \ w ->
    retainObj =<< uiWindow_getRootViewController w

foreign import ccall uiWindow_setRootViewController :: Ptr UIWindowType -> Ptr UIViewControllerType -> MainThread ()
setRootViewController :: UIWindow -> UIViewController -> MainThread ()
setRootViewController wo vco =
  withObjPtr wo $ \ w ->
  withObjPtr vco $ \ vc ->
    uiWindow_setRootViewController w vc

foreign import ccall uiWindow_isKeyWindow :: Ptr UIWindowType -> MainThread CChar
isKeyWindow :: UIWindow -> MainThread Bool
isKeyWindow wo =
  withObjPtr wo $ \ w ->
    unObjcBool <$> uiWindow_isKeyWindow w

foreign import ccall uiWindow_makeKeyAndVisible :: Ptr UIWindowType -> MainThread ()
makeKeyAndVisible :: UIWindow -> MainThread ()
makeKeyAndVisible wo =
  withObjPtr wo $ \ w ->
    uiWindow_makeKeyAndVisible w

foreign import ccall uiWindow_makeKeyWindow :: Ptr UIWindowType -> MainThread ()
makeKeyWindow :: UIWindow -> MainThread ()
makeKeyWindow wo =
  withObjPtr wo $ \ w ->
    uiWindow_makeKeyWindow w

