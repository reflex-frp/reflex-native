{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |Generic target for @UIGestureRecognizer@ which invokes a Haskell callback.
module UIKit.Generic.GestureRecognizerTarget
  (
  -- * Type token and pointer type
    GenericGestureRecognizerTargetType, GenericGestureRecognizerTarget
  -- * Creating
  , new
  -- * Instance methods
  , setRecognizer
  -- * Raw FFI bindings
  , genericGestureRecognizerTarget_new, genericGestureRecognizerTarget_setRecognizer
  ) where

import Control.Monad ((=<<), (<=<))
import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr (Ptr)
import Foreign.StablePtr (StablePtr, deRefStablePtr, newStablePtr)
import ObjC (ObjPtr, ObjType, SafeObjCoerce, retainObj, withObjPtr)
import UIKit.Types (MainThread(..), UIGestureRecognizer, UIGestureRecognizerType, asUIGestureRecognizer)


-- |Type token for the @GenericGestureRecognizerTarget@ class.
data GenericGestureRecognizerTargetType
-- |Pointer to a @GenericGestureRecognizerTarget@ instance.
type GenericGestureRecognizerTarget = ObjPtr GenericGestureRecognizerTargetType
instance SafeObjCoerce GenericGestureRecognizerTargetType ObjType

-- |Raw FFI binding to the ObjC function @genericGestureRecognizerTarget_new@ which creates and configures a @GenericGestureRecognizerTarget@ with the given
-- callback.
foreign import ccall unsafe genericGestureRecognizerTarget_new :: StablePtr (UIGestureRecognizer -> MainThread ()) -> IO (Ptr GenericGestureRecognizerTargetType)

-- |Create a new 'GenericGestureRecognizerTarget' which invokes the given callback whenever it's called by an associated gesture recognizer.
new :: (UIGestureRecognizer -> MainThread ()) -> IO GenericGestureRecognizerTarget
new = retainObj <=< genericGestureRecognizerTarget_new <=< newStablePtr

foreign export ccall genericGestureRecognizerTarget_handler :: StablePtr (UIGestureRecognizer -> MainThread ()) -> Ptr UIGestureRecognizerType -> MainThread ()
-- |Callback invoked from the ObjC code which triggers the configured callback.
genericGestureRecognizerTarget_handler :: StablePtr (UIGestureRecognizer -> MainThread ()) -> Ptr UIGestureRecognizerType -> MainThread ()
genericGestureRecognizerTarget_handler callbackPtr recognizerPtr = do
  callback <- liftIO $ deRefStablePtr callbackPtr
  callback =<< retainObj recognizerPtr

-- |Raw FFI binding which associates a gesture recognizer with the recognizer target.
foreign import ccall unsafe genericGestureRecognizerTarget_setRecognizer :: Ptr GenericGestureRecognizerTargetType -> Ptr UIGestureRecognizerType -> IO ()
-- |Associate a 'UIGestureRecognizer' with the recognizer target. The recognizer will be released when the target is released, which helps the usual case where
-- a target is associated 1:1 with a recognizer and the recognizer is only useful while the target is active. Recognizers do not retain their targets so there
-- is no cycle.
setRecognizer :: SafeObjCoerce recognizer UIGestureRecognizerType => GenericGestureRecognizerTarget -> ObjPtr recognizer -> IO ()
setRecognizer to ro =
  withObjPtr to $ \ t ->
  withObjPtr (asUIGestureRecognizer ro) $ \ r ->
    genericGestureRecognizerTarget_setRecognizer t r

