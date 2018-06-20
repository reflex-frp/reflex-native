-- |Class and instance methods of @UIColor@.
module UIKit.UIColor
  (
  -- * Class methods
    colorWithRedGreenBlueAlpha
  -- * Raw FFI bindings
  , uiColor_colorWithRedGreenBlueAlpha
  ) where

import Control.Monad ((=<<))
import CoreGraphics (CGFloat)
import Foreign.Ptr (Ptr)
import ObjC (retainObj)
import UIKit.Types (UIColor, UIColorType)


-- |Raw FFI binding to @UIColor + colorWithRed:green:blue:alpha:@
foreign import ccall unsafe uiColor_colorWithRedGreenBlueAlpha :: CGFloat -> CGFloat -> CGFloat -> CGFloat -> IO (Ptr UIColorType)
-- |@UIColor + colorWithRed:green:blue:alpha:@ - create a 'UIColor' with the given red, green, blue, and alpha components.
colorWithRedGreenBlueAlpha :: CGFloat -> CGFloat -> CGFloat -> CGFloat -> IO UIColor
colorWithRedGreenBlueAlpha r g b a = retainObj =<< uiColor_colorWithRedGreenBlueAlpha r g b a
