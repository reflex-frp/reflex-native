{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |Types from the @Foundation@ framework, in the @NS@ "namespace".
module Foundation.Types
  (
  -- * Pointer types
    NSArray
  , NSDictionary
  , NSMutableArray
  , NSString
  -- * Pointer type coercions
  , asNSArray
  -- * Type tokens
  , NSArrayType
  , NSDictionaryType
  , NSMutableArrayType
  , NSStringType
  ) where

import ObjC (ObjPtr, ObjType, SafeObjCoerce, coerceObj)


-- |Safely coerce a pointer to an 'NSArray' pointer. See 'SafeObjCoerce'.
asNSArray :: SafeObjCoerce a NSArrayType => ObjPtr a -> NSArray
asNSArray = coerceObj

-- |Type token representing the @NSArray@ class.
data NSArrayType
-- |Pointer to an instance of @NSArray@, ObjC's "immutable" array type. Note that @NSMutableArray@ extends from @NSArray@ and thus a mutable array can be
-- safely coerced to an "immutable" one, and modifications are visible.
type NSArray = ObjPtr NSArrayType
instance SafeObjCoerce NSArrayType ObjType

-- |Type token representing the @NSDictionary@ class.
data NSDictionaryType
-- |Pointer to an instance of @NSDictionary@, ObjC's "immutable" dictionary (key/value mapping) type. Note that @NSMutableDictionary@ extends from
-- @NSDictionary@ and thus a mutable dictionary can be safely coerced to an "immutable" one, and modifications are visible.
type NSDictionary = ObjPtr NSDictionaryType
instance SafeObjCoerce NSDictionaryType ObjType

-- |Type token representing the @NSMutableArray@ class.
data NSMutableArrayType
-- |Pointer to an instance of @NSMutableArray@, ObjC's mutable array type.
type NSMutableArray = ObjPtr NSMutableArrayType
instance SafeObjCoerce NSMutableArrayType ObjType
instance SafeObjCoerce NSMutableArrayType NSArrayType

-- |Type token representing the @NSString@ class.
data NSStringType
-- |Pointer to an instance of @NSString@, ObjC's UTF-16 character string type.
type NSString = ObjPtr NSStringType
instance SafeObjCoerce NSStringType ObjType


