{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |Class and instance methods of @NSMutableArray@ from @Foundation@.
module Foundation.NSMutableArray
  (
  -- * Class methods
    new
  -- * Instance methods
  , addObject
  , addObjectsFromArray
  -- * Raw bindings
  , nsMutableArray_new
  , nsMutableArray_addObject
  , nsMutableArray_addObjectsFromArray
  ) where

import Control.Monad ((=<<))
import Foreign.Ptr (Ptr)
import Foundation.Types (NSArrayType, NSMutableArray, NSMutableArrayType, asNSArray)
import ObjC (ObjPtr, ObjType, SafeObjCoerce, retainObj, asObj, withObjPtr)


-- |Raw FFI binding to @NSMutableArray + (NSMutableArray*)new@
foreign import ccall unsafe nsMutableArray_new :: IO (Ptr NSMutableArrayType)
-- |@NSMutableArray + new@ - create a new mutable ObjC array.
new :: IO NSMutableArray
new = retainObj =<< nsMutableArray_new

-- |Raw FFI binding to @NSMutableArray - addObject:@
foreign import ccall unsafe nsMutableArray_addObject :: Ptr NSMutableArrayType -> Ptr ObjType -> IO ()
-- |@NSMutableArray - addObject:@ - add an object to a mutable array.
addObject :: SafeObjCoerce a ObjType => NSMutableArray -> ObjPtr a -> IO ()
addObject ao oo =
  withObjPtr ao $ \ a ->
  withObjPtr (asObj oo) $ \ o ->
    nsMutableArray_addObject a o

-- |Raw FFI binding to @NSMutableArray - addObjectsFromArray:@
foreign import ccall unsafe nsMutableArray_addObjectsFromArray :: Ptr NSMutableArrayType -> Ptr NSArrayType -> IO ()
-- |@NSMutableArray - addObjectsFromArray
addObjectsFromArray :: SafeObjCoerce array NSArrayType => NSMutableArray -> ObjPtr array -> IO ()
addObjectsFromArray mao ao =
  withObjPtr mao $ \ ma ->
  withObjPtr (asNSArray ao) $ \ a ->
    nsMutableArray_addObjectsFromArray ma a
