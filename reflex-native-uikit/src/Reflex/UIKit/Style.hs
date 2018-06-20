{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
-- |Functions for applying "Reflex.Native" style structures to UIKit views.
module Reflex.UIKit.Style
  (
    -- * Per-view-type functionality
    applyLabelStyle, applyViewStyle
    -- * Per-phase functionality (initial / modify)
  , initialStyle, modifyStyle
  ) where

import Control.Monad (Monad, (<=<))
import Data.Functor.Identity (Identity(..))
import ObjC (ObjPtr, SafeObjCoerce)
import Reflex (Event, Requester(type Request), requesting_)
import Reflex.Native (TextStyle(..), ViewStyle(..))
import Reflex.UIKit.Conversions (makeUIColor, makeUIFont)
import UIKit.Types (MainThread, UILabel, UIViewType)
import qualified UIKit.UILabel as UILabel
import qualified UIKit.UIView as UIView


{-# INLINABLE applyLabelStyle #-}
applyLabelStyle :: Monad m => (forall a. (a -> MainThread ()) -> f a -> m ()) -> UILabel -> TextStyle f -> m ()
applyLabelStyle f l (TextStyle {..}) = do
  f (UILabel.setTextColor l <=< makeUIColor) _textStyle_textColor
  f (UILabel.setFont l <=< makeUIFont) _textStyle_font

{-# INLINABLE applyViewStyle #-}
applyViewStyle :: (SafeObjCoerce v UIViewType, Monad m) => (forall a. (a -> MainThread ()) -> f a -> m ()) -> ObjPtr v -> ViewStyle f -> m ()
applyViewStyle f l (ViewStyle {..}) = do
  f (UIView.setBackgroundColor l <=< makeUIColor) _viewStyle_backgroundColor

{-# INLINABLE initialStyle #-}
initialStyle :: (a -> MainThread ()) -> Identity a -> MainThread ()
initialStyle action (Identity a) = action a

{-# INLINABLE modifyStyle #-}
modifyStyle :: (Requester t m, Request m ~ MainThread) => (a -> MainThread ()) -> Event t a -> m ()
modifyStyle action = requesting_ . fmap action
