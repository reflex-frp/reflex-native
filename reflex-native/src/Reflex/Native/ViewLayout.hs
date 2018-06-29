{-# LANGUAGE DeriveGeneric #-}
-- |Cross platform notion of view layout. FIXME totally lame right now.
module Reflex.Native.ViewLayout
  ( ViewLayout(..)
  ) where

import GHC.Generics (Generic)
import Reflex.Native.Geometry (Rect)


-- |Layout parameters to apply to a view.
data ViewLayout
  = ViewLayout_Fixed !Rect
  -- ^Position the view at an exact location and with an exact size.
  -- The view does not affect the layout of other views, as if it were not there from a layout perspective.
  deriving (Eq, Generic)
