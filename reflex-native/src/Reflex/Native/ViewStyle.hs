-- |Style parameters for all views.
module Reflex.Native.ViewStyle
  ( ViewStyle(..), defaultInitialViewStyle, defaultModifyViewStyle
  ) where

import Data.Functor.Identity (Identity(..))
import Reflex.Class (Event, Reflex, never)
import Reflex.Native.Color (Color, clear)


-- |Style of displayed view parameterized over functor @f@.
--
-- @f ~ Identity@ is used for initial view style where all parameters must be given, while @f ~ Event t@ is used for dynamic modification of view styles after
data ViewStyle f = ViewStyle
  { _viewStyle_backgroundColor :: f Color
  -- ^Background color to draw the view with.
  }

-- |Default 'ViewStyle' for initial display of a view: a transparent background.
defaultInitialViewStyle :: ViewStyle Identity
defaultInitialViewStyle = ViewStyle
  { _viewStyle_backgroundColor = Identity clear
  }

-- |Default 'ViewStyle' for dynamic update, where all parameters 'never' update.
defaultModifyViewStyle :: Reflex t => ViewStyle (Event t)
defaultModifyViewStyle = ViewStyle
  { _viewStyle_backgroundColor = never
  }

