{-# LANGUAGE RecordWildCards #-}
-- |Cross-platform notion of colors.
module Reflex.Native.Color
  (
  -- * @Color@ type
    Color(..)
  -- * @Color@ constants and constructors
  , gray, black, red, green, blue, yellow, cyan, magenta, lightGray, darkGray, white, clear
  ) where

import Text.Show (showString)


-- |Color represented in RGB with an alpha component. Each component should be in the range [0.0 .. 1.0]; values outside of this range are undefined but
-- probably act like their nearest bound.
data Color = Color
  { _color_red :: {-# UNPACK #-} !Double
  -- ^The red value of the color from 0.0 to 1.0 inclusive.
  , _color_green :: {-# UNPACK #-} !Double
  -- ^The green value of the color from 0.0 to 1.0 inclusive.
  , _color_blue :: {-# UNPACK #-} !Double
  -- ^The blue value of the color from 0.0 to 1.0 inclusive.
  , _color_alpha :: {-# UNPACK #-} !Double
  -- ^The alpha value of the color from 0.0 to 1.0 inclusive.
  } deriving (Eq)

-- |Show a 'Color' as @rgba(r,g,b,a)@.
instance Show Color where
  showsPrec _ (Color {..})
    = showString "rgba("
    . shows _color_red . (',':)
    . shows _color_green . (',':)
    . shows _color_blue . (',':)
    . shows _color_alpha
    . (')':)

-- |Construct a neutral gray of the given saturation (0.0 being black and 1.0 being white) and alpha component.
gray :: Double -> Double -> Color
gray value alpha = Color value value value alpha

-- |Opaque black.
black :: Color
black = Color 0 0 0 1

-- |Primary opaque blue.
blue :: Color
blue = Color 0 0 1 1

-- |Completely transparent black (i.e. alpha of 0.0)
clear :: Color
clear = Color 0 0 0 0

-- |Primary opaque cyan (green and blue combined equally).
cyan :: Color
cyan = Color 0 1 1 1

-- |Opaque light gray, equivalent to @'gray' (1/3) 1@
darkGray :: Color
darkGray = gray (1/3) 1

-- |Primary opaque green.
green :: Color
green = Color 0 1 0 1

-- |Opaque light gray, equivalent to @'gray' (2/3) 1@
lightGray :: Color
lightGray = gray (2/3) 1

-- |Primary opaque magenta (red and blue combined equally).
magenta :: Color
magenta = Color 1 0 1 1

-- |Primary opaque red.
red :: Color
red = Color 1 0 0 1

-- |Primary opaque white.
white :: Color
white = Color 1 1 1 1

-- |Primary opaque yellow (red and green combined equally).
yellow :: Color
yellow = Color 1 1 0 1

