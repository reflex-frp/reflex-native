{-# LANGUAGE RecordWildCards #-}
-- |Types corresponding to those in the @CoreGraphics@ framework.
module CoreGraphics
  (
  -- * @CGFloat@
    CGFloat, sizeOfCgFloat
  -- * @CGPoint@
  , CGPoint(..), sizeOfCgPoint
  -- * @CGSize@
  , CGSize(..), sizeOfCgSize
  -- * @CGRect@
  , CGRect(..), sizeOfCgRect
  ) where

import Data.Void (Void)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable(alignment, sizeOf, peek, poke))


-- |Type corresponding to the @CGFloat@ type alias. This binding currently assumes only 64-bit targets, where @CGFloat@ is equivalent to 'Double'.
type CGFloat = Double -- FIXME 64-bit assumption

-- |Size of a 'CGFloat' in bytes, as a short hand for 'sizeOf'.
{-# INLINE sizeOfCgFloat #-}
sizeOfCgFloat :: Int
sizeOfCgFloat = sizeOf (undefined :: CGFloat)

-- |@CGPoint@ - a 2D cartesian point represented by a pair of 'CGFloat's.
data CGPoint = CGPoint
  { _cgPoint_x :: {-# UNPACK #-} !CGFloat
  , _cgPoint_y :: {-# UNPACK #-} !CGFloat
  } deriving (Eq, Show)

-- |Size of a 'CGPoint' in bytes, as a short hand for 'sizeOf'.
{-# INLINE sizeOfCgPoint #-}
sizeOfCgPoint :: Int
sizeOfCgPoint = sizeOfCgFloat * 2

-- |Stores a 'CGPoint' as the X coordinate followed by the Y, conforming to the C @struct@ definition.
instance Storable CGPoint where
  alignment _ = alignment (undefined :: Ptr Void)
  sizeOf _ = sizeOfCgPoint
  peek ptr = CGPoint <$> peek (castPtr ptr) <*> peek (castPtr $ ptr `plusPtr` sizeOfCgFloat)
  poke ptr (CGPoint {..}) = poke (castPtr ptr) _cgPoint_x >> poke (castPtr $ ptr `plusPtr` sizeOfCgFloat) _cgPoint_y

-- |@CGSize@ - a 2D measure of size represented by a pair of @CGFloat@s.
data CGSize = CGSize
  { _cgSize_width  :: {-# UNPACK #-} !CGFloat
  , _cgSize_height :: {-# UNPACK #-} !CGFloat
  } deriving (Eq, Show)

-- |Size of a 'CGSize' in bytes, as a short hand for 'sizeOf'.
{-# INLINE sizeOfCgSize #-}
sizeOfCgSize :: Int
sizeOfCgSize = sizeOfCgFloat * 2

-- |Stores a 'CGSize' as the width dimension followed by the height dimension, conforming to the C @struct@ definition.
instance Storable CGSize where
  alignment _ = alignment (undefined :: Ptr Void)
  sizeOf _ = sizeOfCgSize
  peek ptr = CGSize <$> peek (castPtr ptr) <*> peek (castPtr $ ptr `plusPtr` sizeOfCgFloat)
  poke ptr (CGSize {..}) = poke (castPtr ptr) _cgSize_width >> poke (castPtr $ ptr `plusPtr` sizeOfCgFloat) _cgSize_height

-- |@CGRect@ - a 2D rectangle represented by an origin 'CGPoint' along with a 'CGSize'.
data CGRect = CGRect
  { _cgRect_origin :: {-# UNPACK #-} !CGPoint
  , _cgRect_size   :: {-# UNPACK #-} !CGSize
  } deriving (Eq, Show)

-- |Size of a 'CGRect' in bytes, as a short hand for 'sizeOf'.
{-# INLINE sizeOfCgRect #-}
sizeOfCgRect :: Int
sizeOfCgRect = sizeOfCgPoint + sizeOfCgSize

-- |Stores a 'CGRect' as the origin followed by the size, conforming to the C @struct@ definition.
instance Storable CGRect where
  alignment _ = alignment (undefined :: Ptr Void)
  sizeOf _ = sizeOfCgRect
  peek ptr = CGRect <$> peek (castPtr ptr) <*> peek (castPtr $ ptr `plusPtr` sizeOfCgPoint)
  poke ptr (CGRect {..}) = poke (castPtr ptr) _cgRect_origin >> poke (castPtr $ ptr `plusPtr` sizeOfCgPoint) _cgRect_size

