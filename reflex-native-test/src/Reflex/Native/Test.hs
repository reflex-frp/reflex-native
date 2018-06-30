-- |Consolidation of the @Reflex.Native.Test@ modules into a single namespace and with internals hidden for convenience.
module Reflex.Native.Test (module Export) where

import Reflex.Native.Test.Evaluation as Export
import Reflex.Native.Test.Optics as Export
import Reflex.Native.Test.Runner as Export
import Reflex.Native.Test.Types as Export
  ( TestEvaluation, TestIdentity, tshowTestIdentity, TestViewCommon(..), TestContainerView(..), TestTextView(..), TestView(..)
  , _testView_common, _testView_identity
  )
