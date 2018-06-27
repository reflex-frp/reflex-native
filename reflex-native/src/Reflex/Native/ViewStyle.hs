{-# LANGUAGE RankNTypes #-}
-- |Style parameters for all views.
module Reflex.Native.ViewStyle
  ( ViewStyle(..), defaultInitialViewStyle, defaultModifyViewStyle
  ) where

import Data.Functor.Identity (Identity(..))
import qualified Rank2
import Rank2 (apply)
import Reflex.Class (Event, Reflex, never)
import Reflex.Native.Color (Color, clear)


-- |Style of displayed view parameterized over functor @f@.
--
-- @f ~ Identity@ is used for initial view style where all parameters must be given, while @f ~ Event t@ is used for dynamic modification of view styles after
data ViewStyle f = ViewStyle
  { _viewStyle_backgroundColor :: f Color
  -- ^Background color to draw the view with.
  }

instance Rank2.Functor ViewStyle where
  f <$> ViewStyle a = ViewStyle (f a)
instance Rank2.Apply ViewStyle where
  ViewStyle fa <*> ViewStyle a = ViewStyle (apply fa a)
instance Rank2.Applicative ViewStyle where
  pure f = ViewStyle f
instance Rank2.Foldable ViewStyle where
  foldMap f (ViewStyle a) = f a
instance Rank2.Traversable ViewStyle where
  traverse f (ViewStyle a) = ViewStyle <$> f a

-- |Default 'ViewStyle' for initial display of a view: a transparent background.
defaultInitialViewStyle :: ViewStyle Identity
defaultInitialViewStyle = ViewStyle
  { _viewStyle_backgroundColor = Identity clear
  }

-- |Default 'ViewStyle' for dynamic update, where all parameters 'never' update.
defaultModifyViewStyle :: Reflex t => ViewStyle (Event t)
defaultModifyViewStyle = Rank2.pure never

