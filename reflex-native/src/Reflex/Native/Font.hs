-- |Cross-platform notion of fonts.
module Reflex.Native.Font
  ( Font(..), Weight(..)
  ) where

import Data.Text (Text)


-- ^Identifies a font, either a system font of a particular size and weight or a custom font by name and size.
data Font
  = Font_System Int Weight
  -- ^Whatever the system considers to be the default system font in the chosen size and weight.
  | Font_Custom Text Int
  -- ^A particular font by its name and size.
  deriving (Eq, Show)

-- |A font's weight from lightest to heaviest, corresponding to 100 through 900 of CSS's @font-weight@ property.
data Weight -- FIXME! Order matters to the ObjC code!
  = Weight_UltraLight
  | Weight_Thin
  | Weight_Light
  | Weight_Regular
  | Weight_Medium
  | Weight_Semibold
  | Weight_Bold
  | Weight_Heavy
  | Weight_Black
  deriving (Bounded, Enum, Eq, Ord, Show)
