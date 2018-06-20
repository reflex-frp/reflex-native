-- |Class and instance methods of the @UIPanGestureRecognizer@ class.
module UIKit.UIPanGestureRecognizer
  (
  -- * Class methods
    new
  -- * Instance methods
  , getTranslationInSuperview, getVelocityInSuperview
  -- * Raw FFI bindings
  , uiPanGestureRecognizer_new, uiPanGestureRecognizer_getTranslationInSuperview, uiPanGestureRecognizer_getVelocityInSuperview
  ) where

import Control.Monad ((=<<))
import CoreGraphics (CGPoint)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import ObjC (Obj, ObjType, retainObj, withObjPtr)
import UIKit.Types (UIPanGestureRecognizer, UIPanGestureRecognizerType)


-- |Raw FFI binding to @UIPanGestureRecognizer + new@
foreign import ccall unsafe uiPanGestureRecognizer_new :: Ptr ObjType -> IO (Ptr UIPanGestureRecognizerType)
-- |Create a new 'UIPanGestureRecognizer' with the given target object. The target action is always configured as @handler:@.
new :: Obj -> IO UIPanGestureRecognizer
new oo =
  withObjPtr oo $ \ o ->
    retainObj =<< uiPanGestureRecognizer_new o

-- |Raw FFI binding to @UIPanGestureRecognizer - getTranslationInView:@ with the view given always the superview of the recognizer's associated view.
foreign import ccall unsafe uiPanGestureRecognizer_getTranslationInSuperview :: Ptr UIPanGestureRecognizerType -> Ptr CGPoint -> IO ()
-- |Get the translation recognized by the 'UIPanGestureRecognizer' since the recognizer began recognizing the current gesture expressed in the superview of the
-- recognizer's associated view.
getTranslationInSuperview :: UIPanGestureRecognizer -> IO CGPoint
getTranslationInSuperview ro =
  withObjPtr ro $ \ r ->
    alloca $ \ ptr -> do
      uiPanGestureRecognizer_getTranslationInSuperview r ptr
      peek ptr

-- |Raw FFI binding to @UIPanGestureRecognizer - getVelocityInView:@ with the view given always the superview of the recognizer's associated view.
foreign import ccall unsafe uiPanGestureRecognizer_getVelocityInSuperview :: Ptr UIPanGestureRecognizerType -> Ptr CGPoint -> IO ()
-- |Get the velocity recognized by the 'UIPanGestureRecognizer' since the recognizer began recognizing the current gesture expressed in the superview of the
-- recognizer's associated view.
getVelocityInSuperview :: UIPanGestureRecognizer -> IO CGPoint
getVelocityInSuperview ro =
  withObjPtr ro $ \ r ->
    alloca $ \ ptr -> do
      uiPanGestureRecognizer_getVelocityInSuperview r ptr
      peek ptr
