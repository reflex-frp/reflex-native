{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
-- |Convenient optics for writing tests, built up using @generic-lens@ optics.
module Reflex.Native.Test.Optics
  (
  -- * 'TestView' sum
    _Container, _Marker, _Text
  -- * 'TestContainerView'
  , container_contents, subviews
  -- * 'TestTextView'
  , text_text
  ) where

import Control.Lens (Prism', Lens', Traversal', _Wrapped)
import Data.Functor.Identity (Identity)
import Data.Generics.Product (field)
import Data.Generics.Sum (_Ctor)
import Data.Sequence (Seq)
import Data.Text (Text)
import Reflex.Native.Test.Types (TestMarker, TestView(..), TestContainerView(..), TestTextView(..))


-- |Prism to select a 'TestContainerView' among the constructors of 'TestView'
_Container :: Prism' (TestView v) (TestContainerView v)
_Container = _Ctor @"TestView_Container"

-- |Prism to select a 'TestMarker' among the constructors of 'TestView'
_Marker :: Prism' (TestView v) TestMarker
_Marker = _Ctor @"TestView_Marker"

-- |Prism to select a 'TestTextView' among the constructors of 'TestView'
_Text :: Prism' (TestView v) (TestTextView v)
_Text = _Ctor @"TestView_Text"

-- |Lens to a 'TestContainerView's contained views.
container_contents :: Lens' (TestContainerView Identity) (Seq (TestView Identity))
container_contents = field @"_testContainerView_contents" . _Wrapped

-- |Traversal to visit the contents of any targeted view which happens to be a container
subviews :: Traversal' (TestView Identity) (Seq (TestView Identity))
subviews = _Container . container_contents

-- |Lens to a 'TestTextView's text value.
text_text :: Lens' (TestTextView Identity) Text
text_text = field @"_testTextView_text" . _Wrapped
