{-# LANGUAGE FlexibleContexts #-}
-- |Instance methods of @UIGestureRecognizer@ and related types.
module UIKit.UIGestureRecognizer
  (
  -- * Recognizer state
    UIGestureRecognizerState(..)
  -- * Instance methods
  , getState
  -- * Raw FFI bindings
  , uiGestureRecognizer_getState
  ) where

import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import ObjC (ObjPtr, SafeObjCoerce, withObjPtr)
import UIKit.Types (UIGestureRecognizerType, asUIGestureRecognizer)


-- |Data type reflecting @UIGestureRecognizerState@ from UIKit which represents the various states of a gesture recognizer's state machine.
data UIGestureRecognizerState -- Order matters to the ObjC code!
  = UIGestureRecognizerState_Possible
  -- ^The gesture could be recognized and the recognizer is waiting for events to trigger recognition. Idle and waiting, in other words.
  | UIGestureRecognizerState_Began
  -- ^The beginning of a continuous gesture was recognized.
  | UIGestureRecognizerState_Changed
  -- ^A continuous gesture previously recognized has continued and its parameters changed.
  | UIGestureRecognizerState_Ended
  -- ^A continuous gesture previously recognized has ended or a discrete gesture was recognized.
  | UIGestureRecognizerState_Cancelled
  -- ^A continuous gesture previously recognized has been cancelled.
  | UIGestureRecognizerState_Failed
  -- ^The recognizer received a touch sequence that it can't recognize.
  deriving (Bounded, Enum, Eq, Ord, Show)

-- |Raw FFI binding to @UIGestureRecognizer - getState@.
foreign import ccall unsafe uiGestureRecognizer_getState :: Ptr UIGestureRecognizerType -> IO CInt
-- |@UIGestureRecognizer - getState@ - get the current state of a 'UIGestureRecognizer'.
getState :: SafeObjCoerce recognizer UIGestureRecognizerType => ObjPtr recognizer -> IO UIGestureRecognizerState
getState ro =
  withObjPtr (asUIGestureRecognizer ro) $ \ r ->
    toEnum . fromIntegral <$> uiGestureRecognizer_getState r
