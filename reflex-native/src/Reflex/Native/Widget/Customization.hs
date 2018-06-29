{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |Optics for conveniently customizing view configuration structures, typically used in conjunction with the @widgetWith@ widget building function flavor from
-- "Reflex.Native.Widget.Basic".
--
-- For example:
--
-- @
--   import Control.Lens ((.~))
--   import qualified Reflex.Native.Color as Color
--   import Reflex.Native.Widget.Basic (textWith_)
--   import Reflex.Native.Widget.Customization (backgroundColor, textColor)
--
--   textWith_ (backgroundColor .~ Color.gray . textColor .~ Color.red) "whoa!"
-- @
module Reflex.Native.Widget.Customization
  (
  -- * @Customization@
    Customization(..)
  -- * Customizations for all views
  -- ** View styles
  , backgroundColor, dynBackgroundColor
  -- ** Other view properties
  , accessibilityLabel, dynAccessibilityLabel
  , layout, dynLayout
  -- * Customization for text views
  -- ** Text view styles
  , textColor, dynTextColor
  , textFont, dynTextFont
  -- * Helper classes and functions
  , HasViewConfig(..), initialViewStyle, modifyViewStyle, initialTextStyle, modifyTextStyle
  ) where

import Control.Lens (Lens', set, view)
import Data.Functor.Identity (Identity(..))
import Data.Generics.Product (field)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as Semigroup
import Data.Text (Text)
import Reflex.Class (Dynamic, Event, Reflex, current, leftmost, tag, updated)
import Reflex.Native.Color (Color)
import Reflex.Native.Font (Font)
import Reflex.Native.TextConfig (TextConfig)
import Reflex.Native.TextStyle (TextStyle, defaultModifyTextStyle)
import Reflex.Native.ViewConfig (ViewConfig)
import Reflex.Native.ViewLayout (ViewLayout)
import Reflex.Native.ViewStyle (ViewStyle, defaultModifyViewStyle)


-- |Type which can customize some view configuration structure such as 'ViewConfig' or 'TextConfig'.
--
-- The customization is represented either as a pure function @a -> a@ for customizations which can be applied immediately or as a customization
-- @Event t () -> a -> a@ which relies on a @Dynamic@ and so the resulting view should not be ready immediately but instead at post build time.
data Customization t a
  = Customization_Immediate (a -> a)
  | Customization_PostBuild (Event t () -> a -> a)

-- |Right-biased composition of customizations - for customizations which overlap the rightmost wins.
instance Semigroup (Customization t a) where
  {-# INLINE (<>) #-}
  Customization_Immediate f <> Customization_Immediate g = Customization_Immediate (       g   . f  )
  Customization_Immediate f <> Customization_PostBuild g = Customization_PostBuild (\ e -> g e . f  )
  Customization_PostBuild f <> Customization_Immediate g = Customization_PostBuild (\ e -> g   . f e)
  Customization_PostBuild f <> Customization_PostBuild g = Customization_PostBuild (\ e -> g e . f e)

-- |Right-biased composition of customizations - for customizations which overlap the rightmost wins.
instance Monoid (Customization t a) where
  mempty = Customization_Immediate id
  {-# INLINE mappend #-}
  mappend = (Semigroup.<>)

-- |Set the background color of any view to a static color.
backgroundColor :: HasViewConfig t s => Color -> Customization t s
backgroundColor c = Customization_Immediate $
  set (initialViewStyle . field @"_viewStyle_backgroundColor") (Identity c)

-- |Dynamically set the background color of any view.
dynBackgroundColor :: (Reflex t, HasViewConfig t s) => Dynamic t Color -> Customization t s
dynBackgroundColor d = Customization_PostBuild $ \ pb ->
  modifyViewStyle $ set (field @"_viewStyle_backgroundColor") (leftmost [updated d, tag (current d) pb])

-- |Set the accessibility label of any view to a static value.
accessibilityLabel :: HasViewConfig t s => Text -> Customization t s
accessibilityLabel t = Customization_Immediate $
  set (viewConfig . field @"_viewConfig_initialAccessibilityLabel") (Just t)

-- |Dynamically set the accessibility label of any view.
dynAccessibilityLabel :: (Reflex t, HasViewConfig t s) => Dynamic t (Maybe Text) -> Customization t s
dynAccessibilityLabel d = Customization_PostBuild $ \ pb ->
  set (viewConfig . field @"_viewConfig_setAccessibilityLabel") (Just $ leftmost [updated d, tag (current d) pb])

-- |Set the layout of any view to a static value.
layout :: HasViewConfig t s => ViewLayout -> Customization t s
layout l = Customization_Immediate $
  set (viewConfig . field @"_viewConfig_initialLayout") l

-- |Dynamically set the layout of any view.
dynLayout :: (Reflex t, HasViewConfig t s) => Dynamic t ViewLayout -> Customization t s
dynLayout d = Customization_PostBuild $ \ pb ->
  set (viewConfig . field @"_viewConfig_setLayout") (Just $ leftmost [updated d, tag (current d) pb])

-- |Set the text color of a text view to a static color.
textColor :: Color -> Customization t (TextConfig t)
textColor c = Customization_Immediate $
  set (initialTextStyle . field @"_textStyle_textColor") (Identity c)

-- |Dynamically set the text color of a text view.
dynTextColor :: Reflex t => Dynamic t Color -> Customization t (TextConfig t)
dynTextColor d = Customization_PostBuild $ \ pb ->
  modifyTextStyle $ set (field @"_textStyle_textColor") (leftmost [updated d, tag (current d) pb])

-- |Set the font of a text view to a static color.
textFont :: Font -> Customization t (TextConfig t)
textFont f = Customization_Immediate $
  set (initialTextStyle . field @"_textStyle_font") (Identity f)

-- |Dynamically set the font of a text view.
dynTextFont :: Reflex t => Dynamic t Font -> Customization t (TextConfig t)
dynTextFont d = Customization_PostBuild $ \ pb ->
  modifyTextStyle $ set (field @"_textStyle_font") (leftmost [updated d, tag (current d) pb])

-- |Class to paper over the various view configurations which include a 'ViewConfig'.
class HasViewConfig t a | a -> t where
  viewConfig :: Lens' a (ViewConfig t)

-- |Trivial identity case.
instance HasViewConfig t (ViewConfig t) where
  viewConfig = id

-- |Focus on the 'ViewConfig' inside a 'TextConfig'.
instance HasViewConfig t (TextConfig t) where
  viewConfig = field @"_textConfig_viewConfig"

-- |Focus on the '_viewConfig_initialViewStyle' of any config which has or is a 'ViewConfig'.
initialViewStyle :: HasViewConfig t s => Lens' s (ViewStyle Identity)
initialViewStyle = viewConfig . field @"_viewConfig_initialStyle"

-- |Helper to build a customization which sets a @modifyViewStyle@. @modifyViewStyle@ defaults to @Nothing@; this helper substitutes
-- @Just defaultModifyViewStyle@ then applies the given function.
modifyViewStyle :: forall t s. (Reflex t, HasViewConfig t s) => (ViewStyle (Event t) -> ViewStyle (Event t)) -> s -> s
modifyViewStyle f s =
  let target :: Lens' s (Maybe (ViewStyle (Event t)))
      target = viewConfig . field @"_viewConfig_modifyStyle"
  in set target (Just . f . fromMaybe defaultModifyViewStyle . view target $ s) s

-- |Focus on the '_textConfig_initialTextStyle' of a 'TextConfig'.
initialTextStyle :: Lens' (TextConfig t) (TextStyle Identity)
initialTextStyle = field @"_textConfig_initialStyle"

-- |Helper to build a customization which sets a @modifyTextStyle@. @modifyTextStyle@ defaults to @Nothing@; this helper substitutes
-- @Just defaultModifyTextStyle@ then applies the given function.
modifyTextStyle :: forall t. Reflex t => (TextStyle (Event t) -> TextStyle (Event t)) -> TextConfig t -> TextConfig t
modifyTextStyle f s =
  let target :: Lens' (TextConfig t) (Maybe (TextStyle (Event t)))
      target = field @"_textConfig_modifyStyle"
  in set target (Just . f . fromMaybe defaultModifyTextStyle . view target $ s) s

