{-# LANGUAGE FlexibleContexts #-}
-- |Methods of the @UIAccessibility@ informal protocol.
module UIKit.UIAccessibility
  (
  -- * Instance methods
    setAccessibilityLabel
  -- * Raw FFI bindings
  , uiAccessibility_setAccessibilityLabel
  ) where

import Data.Maybe (Maybe(Just, Nothing))
import Foreign.Ptr (Ptr, nullPtr)
import Foundation.Types (NSString, NSStringType)
import ObjC (ObjPtr, ObjType, SafeObjCoerce, asObj, withObjPtr)
import UIKit.Types (MainThread(..))


-- |Raw FFI binding to @NSObject(UIAccessibility) - setAccessibilityLabel:@
foreign import ccall unsafe uiAccessibility_setAccessibilityLabel :: Ptr ObjType -> Ptr NSStringType -> MainThread ()
-- |@NSObject(UIAccessibility) - setAccessibilityLabel:@ - Set the accessibility label of an accessibility element.
setAccessibilityLabel :: SafeObjCoerce a ObjType => ObjPtr a -> Maybe NSString -> MainThread ()
setAccessibilityLabel ao soMay =
  withObjPtr (asObj ao) $ \ a ->
    case soMay of
      Just so ->
        withObjPtr so $ \ s ->
          uiAccessibility_setAccessibilityLabel a s
      Nothing ->
        uiAccessibility_setAccessibilityLabel a nullPtr
