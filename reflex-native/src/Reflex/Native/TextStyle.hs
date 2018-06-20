-- |Style of displayed text.
module Reflex.Native.TextStyle
  ( TextStyle(..), defaultInitialTextStyle, defaultModifyTextStyle
  ) where

import Data.Functor.Identity (Identity(..))
import Reflex.Class (Event, Reflex, never)
import Reflex.Native.Color (Color, black)
import Reflex.Native.Font (Font(..), Weight(..))


-- |Style of displayed text parameterized over functor @f@.
--
-- @f ~ Identity@ is used for initial text style where all parameters must be given, while @f ~ Event t@ is used for dynamic modification of text styles after
-- initial creation.
data TextStyle f = TextStyle
  { _textStyle_textColor :: f Color
  -- ^The color to display the text with.
  , _textStyle_font :: f Font
  -- ^The font to display the text with.
  }

-- |Default 'TextStyle' for initial display of some text: system font, 12 point, regular weight, and black color.
defaultInitialTextStyle :: TextStyle Identity
defaultInitialTextStyle = TextStyle
  { _textStyle_textColor = Identity black
  , _textStyle_font      = Identity $ Font_System 12 Weight_Regular
  }

-- |Default 'TextStyle' for dynamic update, where all parameters 'never' update.
defaultModifyTextStyle :: Reflex t => TextStyle (Event t)
defaultModifyTextStyle = TextStyle
  { _textStyle_textColor = never
  , _textStyle_font      = never
  }

