{-# LANGUAGE FlexibleContexts #-}
-- |Class and instance methods of the @UILabel@ class.
module UIKit.UILabel
  (
  -- * Class methods
    new
  -- * Instance methods
  , setFont, setText, setTextColor
  -- * Raw FFI bindings
  , uiLabel_new, uiLabel_setFont, uiLabel_setText, uiLabel_setTextColor
  ) where

import Control.Monad ((=<<))
import Foreign.Ptr (Ptr)
import Foundation.Types (NSString, NSStringType)
import ObjC (retainObj, withObjPtr)
import UIKit.Types (MainThread(..), UIColor, UIColorType, UIFont, UIFontType, UILabel, UILabelType)


-- |Raw FFI binding to @UILabel + new@
foreign import ccall unsafe uiLabel_new :: MainThread (Ptr UILabelType)
-- |Create a new 'UILabel' with the default settings.
new :: MainThread UILabel
new = retainObj =<< uiLabel_new

-- |Raw FFI binding to @UILabel - setFont:@
foreign import ccall unsafe uiLabel_setFont :: Ptr UILabelType -> Ptr UIFontType -> MainThread ()
-- |@UILabel - setFont:@ - set the font of the label, overriding any font information given in string attributes if any.
setFont :: UILabel -> UIFont -> MainThread ()
setFont lo fo = 
  withObjPtr lo $ \ l ->
  withObjPtr fo $ \ f ->
    uiLabel_setFont l f

-- |Raw FFI binding to @UILabel - setText:@
foreign import ccall unsafe uiLabel_setText :: Ptr UILabelType -> Ptr NSStringType -> MainThread ()
-- |@UILabel - setText:@ - set the text rendered by the label.
setText :: UILabel -> NSString -> MainThread ()
setText lo so =
  withObjPtr lo $ \ l ->
  withObjPtr so $ \ s ->
    uiLabel_setText l s

-- |Raw FFI binding to @UILabel - setTextColor:@
foreign import ccall unsafe uiLabel_setTextColor :: Ptr UILabelType -> Ptr UIColorType -> MainThread ()
-- |@UILabel - setTextColor:@ - set the color of the label's text, overriding any color information given in the string attributes if any.
setTextColor :: UILabel -> UIColor -> MainThread ()
setTextColor lo co =
  withObjPtr lo $ \ l ->
  withObjPtr co $ \ c ->
    uiLabel_setTextColor l c

