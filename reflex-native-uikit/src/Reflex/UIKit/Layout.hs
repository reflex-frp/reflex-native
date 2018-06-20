{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
-- |Functionality for applying "Reflex.Native" layouts to UIKit views.
module Reflex.UIKit.Layout
  ( applyLayout
  ) where

import CoreGraphics (CGPoint(..), CGRect(..), CGSize(..))
import ObjC (ObjPtr, SafeObjCoerce)
import Reflex.Native (Point(..), Rect(..), Size(..), ViewLayout(..))
import UIKit.Types (MainThread, UIViewType)
import qualified UIKit.UIView as UIView


-- |Apply a 'ViewLayout' to the given view. Used for both initial layout and later updates.
{-# INLINABLE applyLayout #-}
applyLayout :: SafeObjCoerce v UIViewType => ObjPtr v -> ViewLayout -> MainThread ()
applyLayout view = \ case
  ViewLayout_Fixed (Rect (Point {..}) (Size {..})) -> do
    UIView.setFrame view (CGRect (CGPoint _point_x _point_y) (CGSize _size_width _size_height))
