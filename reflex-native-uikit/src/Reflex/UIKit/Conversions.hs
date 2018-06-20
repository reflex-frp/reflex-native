{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
-- |Conversions between "Reflex.Native" and UIKit types.
module Reflex.UIKit.Conversions
  ( makeUIFont, makeUIColor, pointFromCGPoint
  ) where

import Control.Monad ((>>=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import CoreGraphics (CGPoint(..))
import qualified Foundation.NSString as NSString
import Reflex.Native (Color(..), Font(..), Point(..))
import UIKit.Types (UIColor, UIFont)
import qualified UIKit.UIColor as UIColor
import qualified UIKit.UIFont as UIFont


-- |Make a 'UIFont' from a 'Font' using the class methods of "UIKit.UIFont".
{-# INLINABLE makeUIFont #-}
makeUIFont :: MonadIO m => Font -> m UIFont
makeUIFont = liftIO . \ case
  Font_System size weight -> UIFont.systemFontOfSizeWeight (fromIntegral size) (fromIntegral $ fromEnum weight)
  Font_Custom name size -> NSString.fromText name >>= \ ns -> UIFont.fontWithNameSize ns (fromIntegral size)

-- |Make a 'UIColor' from a 'Color' using the class methods of "UIKit.UIColor".
{-# INLINABLE makeUIColor #-}
makeUIColor :: MonadIO m => Color -> m UIColor
makeUIColor (Color {..}) =
  liftIO $ UIColor.colorWithRedGreenBlueAlpha _color_red _color_green _color_blue _color_alpha

-- |Convert a 'CGPoint' into a 'Point'.
{-# INLINABLE pointFromCGPoint #-}
pointFromCGPoint :: CGPoint -> Point
pointFromCGPoint (CGPoint {..}) = Point _cgPoint_x _cgPoint_y
