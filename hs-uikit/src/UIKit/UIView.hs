{-# LANGUAGE FlexibleContexts #-}
-- |Class and instance methods of the @UIView@ class.
module UIKit.UIView
  (
  -- * Instance methods
    addGestureRecognizer, addSubview, removeFromSuperview, setAutoresizesSubviews, setAutoresizingMask, setBackgroundColor, setFrame
  -- * Raw FFI bindings
  , uiView_addGestureRecognizer, uiView_addSubview, uiView_removeFromSuperview, uiView_setAutoresizesSubviews, uiView_setAutoresizingMask
  , uiView_setBackgroundColor, uiView_setFrame
  ) where

import CoreGraphics (CGRect)
import Foreign.C.Types (CChar(..), CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import ObjC (ObjPtr, SafeObjCoerce, objcBool, withObjPtr)
import UIKit.Types
  ( MainThread(..), UIColor, UIColorType, UIGestureRecognizerType, UIViewType, asUIGestureRecognizer, asUIView
  , unsafeUnMainThread
  )


-- |Raw FFI binding to @UIView - addGestureRecognizer:@
foreign import ccall unsafe uiView_addGestureRecognizer :: Ptr UIViewType -> Ptr UIGestureRecognizerType -> MainThread ()
-- |@UIView - addGestureRecognizer:@ - attach the given gesture recognizer to the view so that touch events are passed to the recognizer.
addGestureRecognizer :: (SafeObjCoerce view UIViewType, SafeObjCoerce recognizer UIGestureRecognizerType) => ObjPtr view -> ObjPtr recognizer -> MainThread ()
addGestureRecognizer vo ro =
  withObjPtr (asUIView vo) $ \ v ->
  withObjPtr (asUIGestureRecognizer ro) $ \ r ->
    uiView_addGestureRecognizer v r

-- |Raw FFI binding to @UIView - addSubview:@
foreign import ccall unsafe uiView_addSubview :: Ptr UIViewType -> Ptr UIViewType -> MainThread ()
-- |@UIView - addSubview:@ - add a view to the subviews of the view above all other existing subviews.
addSubview :: (SafeObjCoerce parent UIViewType, SafeObjCoerce subview UIViewType) => ObjPtr parent -> ObjPtr subview -> MainThread ()
addSubview po svo =
  withObjPtr (asUIView po) $ \ p ->
  withObjPtr (asUIView svo) $ \ sv ->
    uiView_addSubview p sv

-- |Raw FFI binding to @UIView - removeFromSuperview@
foreign import ccall unsafe uiView_removeFromSuperview :: Ptr UIViewType -> MainThread ()
-- |@UIView - removeFromSuperview@ - remove a view from the view hierarchy and specifically from its current superview's subviews. No-op if the view does not
-- have a superview.
removeFromSuperview :: SafeObjCoerce v UIViewType => ObjPtr v -> MainThread ()
removeFromSuperview vo =
  withObjPtr (asUIView vo) $ \ v ->
    uiView_removeFromSuperview v

-- |Raw FFI binding to @UIView - setAutoresizesSubviews:@
foreign import ccall unsafe uiView_setAutoresizesSubviews :: Ptr UIViewType -> CChar -> MainThread ()
-- |@UIView - setAutoresizesSubviews:@ - turn on or off the view laying out its subviews according to their autoresizing masks.
setAutoresizesSubviews :: SafeObjCoerce v UIViewType => ObjPtr v -> Bool -> MainThread ()
setAutoresizesSubviews vo b =
  withObjPtr (asUIView vo) $ \ v ->
    uiView_setAutoresizesSubviews v (objcBool b)

-- |Raw FFI binding to @UIView - setAutoresizingMask:@
foreign import ccall unsafe uiView_setAutoresizingMask :: Ptr UIViewType -> CInt -> MainThread ()
-- @UIView - setAutoresizingMask:@ - set the bitvector which controls how the view is laid out in its superview, if the superview has its @autoresizesSubviews@
-- flag turned on.
setAutoresizingMask :: SafeObjCoerce v UIViewType => ObjPtr v -> CInt -> MainThread ()
setAutoresizingMask vo m =
  withObjPtr (asUIView vo) $ \ v ->
    uiView_setAutoresizingMask v m

-- |Raw FFI binding to @UIView - setBackgroundColor:@
foreign import ccall unsafe uiView_setBackgroundColor :: Ptr UIViewType -> Ptr UIColorType -> MainThread ()
-- |@UIView - setBackgroundColor:@ - set the background color of the view. Automatically sets @isOpaque@ based on the alpha component of the color.
-- property appropriately.
setBackgroundColor :: SafeObjCoerce v UIViewType => ObjPtr v -> UIColor -> MainThread ()
setBackgroundColor vo co =
  withObjPtr (asUIView vo) $ \ v ->
  withObjPtr co $ \ c ->
    uiView_setBackgroundColor v c

-- |Raw FFI binding to @UIView - setFrame:@
foreign import ccall unsafe uiView_setFrame :: Ptr UIViewType -> Ptr CGRect -> MainThread ()
-- |@UIView - setFrame:@ - set the frame (position and size) of the view in the coordinate system of its superview.
setFrame :: SafeObjCoerce v UIViewType => ObjPtr v -> CGRect -> MainThread ()
setFrame vo rect =
  withObjPtr (asUIView vo) $ \ v ->
    MainThread . alloca $ \ ptr -> do
      poke ptr rect
      unsafeUnMainThread $ uiView_setFrame v ptr
