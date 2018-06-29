{-# LANGUAGE ExplicitNamespaces #-}
-- |Basic widgets based on the 'ViewBuilder' abstraction.
--
-- Each type of widget usually has several flavors, though not every widget has every flavor. The flavors are:
--
--    * @widget@ - default styled version, returning the created view.
--    * @widget_@ - default styled version, returning @()@ or your action's @a@ only.
--    * @widgetWith@ - version customized as given, returning the created view. See "Reflex.Native.Widget.Customization" for the 'Customization' type and
--    various constructors for it.
--    * @widgetWith_@ - customized widget returning @()@ or @a@.
--    constructors for it.
--    * @dynWidget@ - for widgets that have some displayed value, the version without @dyn@ shows a static value while the @dyn@ version tracks a 'Dynamic'
--    over time and returns the created view. See "Reflex.Native.Test.Widget.Basic#dynTiming".
--    * @dynWidget_@ - dynamic valued widget returning @()@ or @a@.
--    * @dynWidgetWith@ - dynamic valued widget with customizations.
--    * @dynWidgetWith_@ - dynamic valued widget with customzations returning @()@ or @a@.
--
-- #dynTiming#
-- == Timing of @dyn@ widgets
--
-- To avoid accidental causality loops, these versions always delay their initial display until the next frame though this should be unnoticeable - see
-- 'notReadyUntil'.
module Reflex.Native.Widget.Basic
  (
  -- * Plain container views
    container, container_, containerWith, containerWith_
  -- * Test display views
  , text, text_, textWith, textWith_, dynText, dynText_, dynTextWith, dynTextWith_
  ) where

import Data.Functor (void)
import Data.Text (Text)
import Reflex.Class (Dynamic, current, leftmost, tag, updated)
import Reflex.Native.TextConfig (TextConfig(..), defaultTextConfig)
import Reflex.Native.Widget.Customization (Customization(..))
import Reflex.Native.ViewBuilder.Class (TextView, View, ViewBuilder(type ViewBuilderSpace, buildView, buildTextView))
import Reflex.Native.ViewConfig (ViewConfig(..), defaultViewConfig)
import Reflex.NotReady.Class (NotReady, notReadyUntil)
import Reflex.PostBuild.Class (PostBuild, getPostBuild)


-- |Build a plain container view with some hierarchy inside and the 'defaultViewConfig'.
container
  :: (NotReady t m, PostBuild t m, ViewBuilder t m)
  => m a
  -- ^The child hierarchy to build inside the container.
  -> m (a, View (ViewBuilderSpace m) t)
container = containerWith mempty

-- |Build a plain container view with some hierarchy inside and the 'defaultViewConfig'.
container_
  :: (NotReady t m, PostBuild t m, ViewBuilder t m)
  => m a
  -- ^The child hierarchy to build inside the container.
  -> m a
container_ = fmap fst . container

-- |Build a plain container view with some hierarchy inside and the 'defaultViewConfig' but with the 'ViewStyle' tweaked using the given function.
containerWith
  :: (NotReady t m, PostBuild t m, ViewBuilder t m)
  => Customization t (ViewConfig t)
  -- ^The customization to apply. See "Reflex.Native.Widget.Customization".
  -> m a
  -- ^The child hierarchy to build inside the container.
  -> m (a, View (ViewBuilderSpace m) t)
containerWith customization body = case customization of
  Customization_Immediate f -> buildView (f defaultViewConfig) body
  Customization_PostBuild f -> do
    pb <- getPostBuild
    notReadyUntil pb
    buildView (f pb defaultViewConfig) body

-- |Build a plain container view with some hierarchy inside and the 'defaultViewConfig' but with the 'ViewStyle' tweaked using the given function.
containerWith_
  :: (NotReady t m, PostBuild t m, ViewBuilder t m)
  => Customization t (ViewConfig t)
  -- ^The customization to apply. See "Reflex.Native.Widget.Customization".
  -> m a
  -- ^The child hierarchy to build inside the container.
  -> m a
containerWith_ f = fmap fst . containerWith f

-- |Build a plain static text view with the given text and the 'defaultTextConfig'.
text
  :: (NotReady t m, PostBuild t m, ViewBuilder t m)
  => Text
  -- ^The text to display.
  -> m (TextView (ViewBuilderSpace m) t)
text = textWith mempty

-- |Build a plain static text view with the given text and the 'defaultTextConfig'.
text_
  :: (NotReady t m, PostBuild t m, ViewBuilder t m)
  => Text
  -- ^The text to display.
  -> m ()
text_ = void . text

-- |Build a static text view with the given text and the 'defaultTextConfig' customized using the given function.
textWith
  :: (NotReady t m, PostBuild t m, ViewBuilder t m)
  => Customization t (TextConfig t)
  -- ^The customization to apply. See "Reflex.Native.Widget.Customization".
  -> Text
  -- ^The text to display.
  -> m (TextView (ViewBuilderSpace m) t)
textWith customization t = case customization of
  Customization_Immediate f -> buildTextView (f $ defaultTextConfig { _textConfig_initialText = t })
  Customization_PostBuild f -> do
    pb <- getPostBuild
    notReadyUntil pb
    buildTextView (f pb $ defaultTextConfig { _textConfig_initialText = t })

-- |Build a static text view with the given text and the 'defaultTextConfig' customized using the given function.
textWith_
  :: (NotReady t m, PostBuild t m, ViewBuilder t m)
  => Customization t (TextConfig t)
  -- ^The customization to apply. See "Reflex.Native.Widget.Customization".
  -> Text
  -- ^The text to display.
  -> m ()
textWith_ f t = void $ textWith f t

-- |Build a plain dynamic text view with the given dynamically updating text and the 'defaultTextConfig'.
dynText
  :: (NotReady t m, PostBuild t m, ViewBuilder t m)
  => Dynamic t Text
  -- ^The text to display over time.
  -> m (TextView (ViewBuilderSpace m) t)
dynText = dynTextWith mempty

-- |Build a plain dynamic text view with the given dynamically updating text and the 'defaultTextConfig'.
dynText_
  :: (NotReady t m, PostBuild t m, ViewBuilder t m)
  => Dynamic t Text
  -- ^The text to display over time.
  -> m ()
dynText_ = void . dynText

-- |Build a dynamic text view with the given dynamically updating text and the 'defaultTextConfig' customized using the given function.
dynTextWith
  :: (NotReady t m, PostBuild t m, ViewBuilder t m)
  => Customization t (TextConfig t)
  -- ^The customization to apply. See "Reflex.Native.Widget.Customization".
  -> Dynamic t Text
  -- ^The text to display over time.
  -> m (TextView (ViewBuilderSpace m) t)
dynTextWith customization dt = do
  pb <- getPostBuild
  let f = case customization of
        Customization_Immediate g -> g
        Customization_PostBuild g -> g pb
  notReadyUntil pb
  buildTextView . f $ defaultTextConfig { _textConfig_setText = Just $ leftmost [updated dt, tag (current dt) pb] }

-- |Build a dynamic text view with the given dynamically updating text and the 'defaultTextConfig' customized using the given function.
dynTextWith_
  :: (NotReady t m, PostBuild t m, ViewBuilder t m)
  => Customization t (TextConfig t)
  -- ^The customization to apply. See "Reflex.Native.Widget.Customization".
  -> Dynamic t Text
  -- ^The text to display over time.
  -> m ()
dynTextWith_ f dt = void $ dynTextWith f dt

