{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
-- |Cross-platform notion of gestures and recognition thereof.
module Reflex.Native.Gesture
  (
  -- * Gesture types and recognition parameters
    GestureType(..), GestureSpec(..)
  -- * Recognition states and data
  , GestureData, PanGesture(..), GestureState(..), _gestureState_data
  ) where

import Data.Maybe (Maybe(Just, Nothing))
import Reflex.Native.Geometry (Point)


-- |Represents the types of gesture that can be recognized. Typically used as a kind via @DataKinds@.
data GestureType
  = GestureType_Pan
  -- ^Pan (or drag) gestures where a touch starts and is moved while being held. Continuous.
  deriving (Eq, Show)

-- |Specification of recognition parameters for each 'GestureType'
data GestureSpec (gt :: GestureType) where
  GestureSpec_Pan :: GestureSpec 'GestureType_Pan

-- |Type family mapping 'GestureType' to data for the recognized gesture.
type family GestureData (gt :: GestureType) where
  GestureData 'GestureType_Pan = PanGesture

-- |Data about a pan gesture ('GestureType_Pan' / 'GestureSpec_Pan') in progress.
data PanGesture = PanGesture
  { _panGesture_translation :: {-# UNPACK #-} !Point -- FIXME the "relative to began state" thing is obnoxious.
  -- ^Translation of the pan gesture currently. Only useful in relation to the translation reported with 'GestureState_Began'.
  , _panGesture_velocity    :: {-# UNPACK #-} !Point
  -- ^Velocity of the pan gesture currently.
  } deriving (Eq, Show)

-- |State of a gesture, from not recognized (@GestureState_None@) through various states of recognition.
--
-- Gestures are either discrete where they immediately go from @GestureState_None@ to @GestureState_Ended@, or continuous where they go through the full range
-- of states.
data GestureState a
  = GestureState_None
  -- ^The gesture is not presently recognized to be occurring.
  | GestureState_Began a
  -- ^The continuous gesture has just been recognized.
  | GestureState_Changed a
  -- ^The continuous gesture has changed but not yet ended.
  | GestureState_Ended a
  -- ^The discrete gesture was recognized or the continuous gesture finished.
  | GestureState_Cancelled
  -- ^The continuous gesture was cancelled after beginning.
  deriving (Eq, Functor, Show)

-- |Project the gesture data @a@ from a @GestureState a@ if it's holding gesture data (i.e. is @GestureState_Began@, @GestureState_Changed@, or
-- @GestureState_Ended@).
_gestureState_data :: GestureState a -> Maybe a
_gestureState_data = \ case
  GestureState_None      -> Nothing
  GestureState_Began a   -> Just a
  GestureState_Changed a -> Just a
  GestureState_Ended a   -> Just a
  GestureState_Cancelled -> Nothing
