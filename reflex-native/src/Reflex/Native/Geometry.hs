{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
-- |Cross-platform notions of geometry, such as points and rectangles.
module Reflex.Native.Geometry
  ( Point(..), Size(..), Rect(..) ) where

import Data.AdditiveGroup (AdditiveGroup(zeroV, (^+^), negateV, (^-^)))
import Data.VectorSpace (VectorSpace(type Scalar, (*^)))
import GHC.Generics (Generic)


-- |2D point represented as X and Y coordinates given as @Double@s.
--
-- Uses the typical computer graphics flipped cartesian system, i.e. positive X towards the right and positive Y downwards.
data Point = Point
  { _point_x :: !Double
  , _point_y :: !Double
  } deriving (Eq, Generic)

-- |Show a 'Point' as @(x,y)@
instance Show Point where
  showsPrec _ (Point {..})
    = ('(':)
    . shows _point_x . (',':)
    . shows _point_y . (')':)

-- |Points form an additive group by distribution, e.g. @Point x1 y1 ^+^ Point x2 y2 == Point (x1 ^+^ x2) (y1 ^+^ y2)@.
instance AdditiveGroup Point where
  zeroV = Point 0 0
  negateV (Point x y) = Point (negate x) (negate y)
  Point x1 y1 ^+^ Point x2 y2 = Point (x1 + x2) (y1 + y2)
  Point x1 y1 ^-^ Point x2 y2 = Point (x1 - x2) (y1 - y2)

-- |Points form a vector space with the scalar being a @Double@, e.g. @f *^ Point x y == Point (f *^ x) (f *^ y)@.
instance VectorSpace Point where
  type Scalar Point = Double
  f *^ Point x y = Point (x*f) (y*f)

-- |2D size represented as width and height dimensions given as @Double@s.
data Size = Size
  { _size_width  :: !Double
  , _size_height :: !Double
  } deriving (Eq, Generic)

-- |Show a 'Size' as @(wxh)@.
instance Show Size where
  showsPrec _ (Size {..})
    = ('(':)
    . shows _size_width . ('x':)
    . shows _size_height . (')':)

-- |Sizes form an additive group by distribution, e.g. @Size w1 h1 ^+^ Size w2 h2 == Size (w1 ^+^ w2) (h1 ^+^ h2)@.
instance AdditiveGroup Size where
  zeroV = Size 0 0
  negateV (Size x y) = Size (negate x) (negate y)
  Size x1 y1 ^+^ Size x2 y2 = Size (x1 + x2) (y1 + y2)
  Size x1 y1 ^-^ Size x2 y2 = Size (x1 - x2) (y1 - y2)

-- |Sizes form a vector space with the scalar being a @Double@, e.g. @f *^ Size w h == Size (f *^ w) (f *^ h)@.
instance VectorSpace Size where
  type Scalar Size = Double
  f *^ Size w h = Size (w*f) (h*f)

-- |2D rectangles represented by an origin point and a size.
data Rect = Rect
  { _rect_origin :: !Point
  , _rect_size   :: !Size
  } deriving (Eq, Generic)

-- |Show a 'Rect' as @(x,y)+(wxh)@
instance Show Rect where
  showsPrec _ (Rect {..}) = shows _rect_origin . ('+':) . shows _rect_size
