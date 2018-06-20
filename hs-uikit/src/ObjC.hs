{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |Basic types and functionality for interoperating with Objective-C for using UIKit.
--
-- Objective-C is a reference counted object oriented language with classes, objects which are instances of those classes, and protocols describing common
-- behavior among classes.
--
-- Objective-C classes are represented by type tokens, data types with no constructors used as phantom type parameters to 'ObjPtr's which represent pointers
-- to object instances. Protocols are represented as classes since they only describe what methods can be used on an instance like classes do and are otherwise
-- an ObjC implementation detail.
--
-- Objective-C supports nominal subtyping by way of classes extending other classes. This relationship is embedded on the Haskell side along with protocol
-- conformance using the @'SafeObjType' from to@ typeclass, whose instances describe which ObjC types are subtypes of others. 'coerceObj' allows for safe
-- coercion of a subtype to its supertype, while 'downCastObj' allows for explicit (possibly unsafe) casting from a supertype to a subtype.
--
-- Objective-C reference counting is supported via 'ObjPtr' which wraps a pointer to the Objective-C object along with retainment characteristics.
module ObjC
  (
  -- * Object pointers and coercions
    ObjPtr(..), withObjPtr
  , SafeObjCoerce, coerceObj, downCastObj
  -- * @id@ / @NSObject@
  , ObjType, Obj, asObj
  -- * @BOOL@
  , yes, no, objcBool, unObjcBool
  -- * Reference counting
  , retainObj
  -- * Raw FFI bindings
  , hsobjc_retain, hsobjc_release
  ) where

import Control.Monad ((=<<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith, restoreM)
import Data.Bool (Bool(True, False), bool)
import Foreign.C.Types (CChar)
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)


-- |Reference to an Objective-C object of type @a@, a type token representing a class or protocol.
--
-- Presently only has one retainment style, @ObjPtr_Owned@, where the retain count was incremented when the object was adopted by the Haskell RTS which will be
-- released when the @ObjPtr@ is finalized by the Haskell GC. Might be extended in the future to support borrowing where the retain count is not incremented,
-- as that's more memory efficient for callbacks and similar albeit more dangerous.
newtype ObjPtr a = ObjPtr_Owned { _objPtr_value :: ForeignPtr a }

-- |Execute some 'IO' action using the actual 'Ptr' represented by an 'ObjPtr'.
--
-- __Caution:__ Do not refer to the @Ptr@ outside of the action, as the reference to the @ObjPtr@ might have gone out of scope and the pointer finalized,
-- causing the referent to be released. See 'ForeignPtr' for more.
{-# INLINABLE withObjPtr #-}
withObjPtr :: MonadBaseControl IO m => ObjPtr a -> (Ptr a -> m b) -> m b
withObjPtr op = \ action ->
  restoreM =<< liftBaseWith (\ runInBase -> withForeignPtr (_objPtr_value op) (runInBase . action))

-- |Type token for any Objective-C object, usually conforming to @NSObject@.
data ObjType
-- |Pointer to some Objective-C object, analogous to Objective-C's @id@ type.
type Obj = ObjPtr ObjType

-- |Typeclass whose instances denote types which are subtypes of others and thus can be safely coerced. For example, @SafeObjCoerce NSStringType ObjType@
-- denotes that @NSString@ is a subtype of @id@ and thus can be safely coerced in that direction.
class SafeObjCoerce from to
-- |Any type is trivially and safely coerced to itself.
instance {-# OVERLAPPABLE #-} SafeObjCoerce a a

-- |Coerce an @'ObjPtr' a@ to an @'ObjPtr' b@ if an instance @'SafeObjCoerce' a b@ exists indicating that it's a safe coercion to perform.
{-# INLINE coerceObj #-}
coerceObj :: SafeObjCoerce from to => ObjPtr from -> ObjPtr to
coerceObj = ObjPtr_Owned . castForeignPtr . _objPtr_value

-- |Cast an @'ObjPtr' a@ to an @'ObjPtr' b@ if an instance @'SafeObjCoerce' b a@ indicates that there is some possible subtyping relationship present. This is
-- unchecked at compile time and potentially unsafe as it's not known except at runtime whether the pointer really points to an instance conforming to @b@.
{-# INLINE downCastObj #-}
downCastObj :: SafeObjCoerce to from => ObjPtr from -> ObjPtr to
downCastObj = ObjPtr_Owned . castForeignPtr . _objPtr_value

-- |Raw FFI binding to @NSObject - retain@ by way of @CFBridgingRetain@. __Warning:__ while the typical naming strategy for binding functions is
-- @haskellModule_method@, it is crucial this does not get named @objc_retain@, as the linker will silently replace the ObjC runtime function of the same name
-- and then all calls to @retain@ will infinite loop.
foreign import ccall hsobjc_retain :: Ptr a -> IO ()
-- |Raw FFI binding to @NSObject - release@ by way of @CFBridgingRelease@. __Warning:__ while the typical naming strategy for binding functions is
-- @haskellModule_method@, it is crucial this does not get named @objc_release@, as the linker will silently replace the ObjC runtime function of the same name
-- and then all calls to @release@ will infinite loop.
foreign import ccall "&hsobjc_release" hsobjc_release :: FunPtr (Ptr a -> IO ())

-- |Increment the reference counter of an Objective-C object using 'hsobjc_retain' and then wrap it in an 'ObjPtr' which will decrement the reference later
-- when the @ObjPtr@ is no longer referred to.
{-# INLINABLE retainObj #-}
retainObj :: (MonadIO m, SafeObjCoerce a ObjType) => Ptr a -> m (ObjPtr a)
retainObj p = liftIO $ do
  hsobjc_retain p
  ObjPtr_Owned <$> newForeignPtr hsobjc_release p

-- |Safely coerce any Objective-C object to 'Obj'.
{-# INLINABLE asObj #-}
asObj :: SafeObjCoerce a ObjType => ObjPtr a -> Obj
asObj = coerceObj

-- |Constant corresponding to Objective-C's @NO@ of type @BOOl@ (a type alias for @char@).
{-# INLINABLE no #-}
no :: CChar
no  = 0

-- |Constant corresponding to Objective-C's @NO@ of type @BOOl@ (a type alias for @char@).
{-# INLINABLE yes #-}
yes :: CChar
yes = 1

-- |Convert a Haskell 'Bool' to an Objective-C @BOOL@ ('CChar').
{-# INLINABLE objcBool #-}
objcBool :: Bool -> CChar
objcBool = bool no yes

-- |Convert an Objective-C @BOOL@ ('CChar') to a Haskell 'Bool'.
{-# INLINABLE unObjcBool #-}
unObjcBool :: CChar -> Bool
unObjcBool c = if c == 0 then False else True

