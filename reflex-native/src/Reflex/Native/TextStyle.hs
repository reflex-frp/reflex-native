{-# LANGUAGE RankNTypes #-}
-- |Style of displayed text.
module Reflex.Native.TextStyle
  ( TextStyle(..), defaultInitialTextStyle, defaultModifyTextStyle
  ) where

import Data.Functor.Identity (Identity(..))
import Data.Monoid ((<>))
import qualified Rank2
import Rank2 (apply)
import Reflex.Class (Event, Reflex, never)
import Reflex.Native.Color (Color, black)
import Reflex.Native.Font (Font(..), Weight(..))


-- |Style of displayed text parameterized over functor @f@.
--
-- @f ~ Identity@ is used for initial text style where all parameters must be given, while @f ~ Event t@ is used for dynamic modification of text styles after
-- initial creation.
data TextStyle v = TextStyle
  { _textStyle_textColor :: v Color
  -- ^The color to display the text with.
  , _textStyle_font :: v Font
  -- ^The font to display the text with.
  }

instance Rank2.Functor TextStyle where
  f <$> TextStyle a b = TextStyle (f a) (f b)
instance Rank2.Apply TextStyle where
  TextStyle fa fb <*> TextStyle a b = TextStyle (apply fa a) (apply fb b)
instance Rank2.Applicative TextStyle where
  pure f = TextStyle f f
instance Rank2.Foldable TextStyle where
  foldMap f (TextStyle a b) = f a <> f b
instance Rank2.Traversable TextStyle where
  traverse f (TextStyle a b) = TextStyle <$> f a <*> f b

-- |Default 'TextStyle' for initial display of some text: system font, 12 point, regular weight, and black color.
defaultInitialTextStyle :: TextStyle Identity
defaultInitialTextStyle = TextStyle
  { _textStyle_textColor = Identity black
  , _textStyle_font      = Identity $ Font_System 12 Weight_Regular
  }

-- |Default 'TextStyle' for dynamic update, where all parameters 'never' update.
defaultModifyTextStyle :: Reflex t => TextStyle (Event t)
defaultModifyTextStyle = Rank2.pure never

