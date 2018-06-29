{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
-- |Configuration of any type of view.
module Reflex.Native.ViewConfig
  (
    -- * Configuration for regular views created with Reflex Native
    ViewConfig(..), defaultViewConfig
    -- * Configuration for other views adopted by Reflex Native
  , RawViewConfig(..), defaultRawViewConfig, viewConfigToRawViewConfig
  ) where

import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe(Nothing))
import Data.Text (Text)
import GHC.Generics (Generic)
import Reflex.Class (Event)
import Reflex.Native.Geometry (Rect(..), Point(..), Size(..))
import Reflex.Native.ViewLayout (ViewLayout(..))
import Reflex.Native.ViewStyle (ViewStyle, defaultInitialViewStyle)


-- |Configuration of any type of view created by Reflex native, including its style, layout, and accessibility parameters.
data ViewConfig t = ViewConfig
  { _viewConfig_initialStyle :: ViewStyle Identity
  -- ^Style to initially use when displaying the view.
  , _viewConfig_modifyStyle :: Maybe (ViewStyle (Event t))
  -- ^Optional @Event@s to dynamically update the view style after initial display.
  , _viewConfig_initialLayout :: ViewLayout
  -- ^Initial layout for the view.
  , _viewConfig_modifyLayout :: Maybe (Event t ViewLayout)
  -- ^Optional @Event@ to update the layout of a view dynamically.
  , _viewConfig_initialAccessibilityLabel :: Maybe Text
  -- ^Initial accessibility label to apply to the view.
  , _viewConfig_modifyAccessibilityLabel :: Maybe (Event t (Maybe Text))
  -- ^Optional @Event@ to dynamically update accessiblity label after initial display.
  } deriving (Generic)

-- |Default 'ViewConfig' with the 'defaultInitialViewStyle', fixed 0x0+0x0 layout, and no dynamically updating anything.
defaultViewConfig :: ViewConfig t
defaultViewConfig = ViewConfig
  { _viewConfig_initialStyle              = defaultInitialViewStyle
  , _viewConfig_modifyStyle               = Nothing
  , _viewConfig_initialLayout             = ViewLayout_Fixed (Rect (Point 0 0) (Size 0 0))
  , _viewConfig_modifyLayout              = Nothing
  , _viewConfig_initialAccessibilityLabel = Nothing
  , _viewConfig_modifyAccessibilityLabel  = Nothing
  }

-- |Configuration of a raw view created outside Reflex Native and then adopted using 'Reflex.Native.ViewBuilder.Class.wrapRawView' or similar. Allows dynamic
-- update of a view just like 'ViewConfig', but not the initial setting.
data RawViewConfig t = RawViewConfig
  { _rawViewConfig_modifyStyle :: Maybe (ViewStyle (Event t))
  -- ^Optional @Event@s to dynamically update the view style after initial display.
  , _rawViewConfig_modifyLayout :: Maybe (Event t ViewLayout)
  -- ^Optional @Event@ to update the layout of a view dynamically.
  , _rawViewConfig_modifyAccessibilityLabel :: Maybe (Event t (Maybe Text))
  -- ^Optional @Event@ to dynamically update accessiblity label after initial display.
  } deriving (Generic)

-- |Default 'RawViewConfig' which never dynamically updates anything.
defaultRawViewConfig :: RawViewConfig t
defaultRawViewConfig = RawViewConfig
  { _rawViewConfig_modifyStyle              = Nothing
  , _rawViewConfig_modifyLayout             = Nothing
  , _rawViewConfig_modifyAccessibilityLabel = Nothing
  }

-- |Extract the equivalent 'RawViewConfig' for some 'ViewConfig'.
viewConfigToRawViewConfig :: ViewConfig t -> RawViewConfig t
viewConfigToRawViewConfig (ViewConfig {..}) = RawViewConfig
  { _rawViewConfig_modifyStyle              = _viewConfig_modifyStyle
  , _rawViewConfig_modifyLayout             = _viewConfig_modifyLayout
  , _rawViewConfig_modifyAccessibilityLabel = _viewConfig_modifyAccessibilityLabel
  }

