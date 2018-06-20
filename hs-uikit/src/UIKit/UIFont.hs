-- |Class and instance methods of the @UIFont@ class.
module UIKit.UIFont
  (
  -- * Class methods
    systemFontOfSizeWeight, fontWithNameSize
  -- * Raw FFI bindings
  , uiFont_systemFontOfSizeWeight, uiFont_fontWithNameSize
  ) where

import Control.Monad ((=<<))
import CoreGraphics (CGFloat)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Foundation (NSString, NSStringType)
import ObjC (retainObj, withObjPtr)
import UIKit.Types (UIFont, UIFontType)


-- |Raw FFI binding to @UIFont + systemFontOfSize:weight:@
foreign import ccall unsafe uiFont_systemFontOfSizeWeight :: CGFloat -> CInt -> IO (Ptr UIFontType)
-- |@UIFont + systemFontOfSize:weight:@ - create or obtain a cached instance of 'UIFont' for the system font of the given size in points and weight. Weights
-- are represented as [0..8] ranging from ultra light to black corresponding to @UIFontWeight@, and should be changed to a proper data type.
systemFontOfSizeWeight :: CGFloat -> CInt -> IO UIFont
systemFontOfSizeWeight s w = retainObj =<< uiFont_systemFontOfSizeWeight s w

-- |Raw FFI binding to @UIFont + fontWithName:size:@
foreign import ccall unsafe uiFont_fontWithNameSize :: Ptr NSStringType -> CGFloat -> IO (Ptr UIFontType)
-- |@UIFont + fontWithName:size:@ - create or obtain a cached instance of 'UIFont' for the given font name and size.
fontWithNameSize :: NSString -> CGFloat -> IO UIFont
fontWithNameSize nameo size =
  withObjPtr nameo $ \ name ->
    retainObj =<< uiFont_fontWithNameSize name size
