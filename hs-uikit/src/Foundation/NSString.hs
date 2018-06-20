module Foundation.NSString where

import Control.Monad ((=<<))
import Data.Text (Text)
import qualified Data.Text.Foreign as DTF
import Data.Word (Word16)
import Foreign.C.Types (CULong(..))
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
import Foundation.Types (NSString, NSStringType)
import ObjC (retainObj, withObjPtr)


foreign import ccall unsafe nsString_stringWithCharactersLength :: Ptr Word16 -> CULong -> IO (Ptr NSStringType)

fromText :: Text -> IO NSString
fromText text =
  DTF.useAsPtr text $ \ charsPtr len ->
    retainObj =<< nsString_stringWithCharactersLength charsPtr (fromIntegral len)

foreign import ccall unsafe nsString_length :: Ptr NSStringType -> IO CULong
foreign import ccall unsafe nsString_getCharacters :: Ptr NSStringType -> Ptr Word16 -> IO ()

toText :: NSString -> IO Text
toText so =
  withObjPtr so $ \ s -> do
    len <- nsString_length s
    allocaArray (fromIntegral len) $ \ charsPtr -> do
      nsString_getCharacters s charsPtr
      DTF.fromPtr charsPtr (fromIntegral len)
