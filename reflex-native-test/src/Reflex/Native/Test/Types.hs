{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |Types used throughout "Reflex.Native.Test".
module Reflex.Native.Test.Types
  (
  -- * Unique identities
    TestIdentity, unTestIdentity, newTestIdentity, tshowTestIdentity
  -- * Test views
  , TestHolder, TestViewCommon(..), TestContainerView(..), TestTextView(..), TestMarker(..), TestView(..), _testView_common, _testView_identity
  -- ** Test views as diagnostic text
  , showsTestContainerView, showsTestView, showTestViewHierarchy, tshowTestContainerViewIdentity, tshowTestTextViewIdentity, tshowTestMarkerIdentity
  , tshowTestViewIdentity
  -- ** Traversing a test view hierarchy
  , traverseTestContainerView, traverseTestView
  -- * Test execution environment and evaluation monad
  , TestEnv(..), TestEvaluation(..)
  ) where

import Control.Concurrent.Chan (Chan)
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Exception (MonadAsyncException, MonadException)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Strict (RWST)
import Data.Dependent.Sum (DSum)
import  Data.DList (DList)
import qualified Data.DList as DList
import Data.Foldable (toList)
import Data.Functor.Identity (Identity(..))
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Monoid ((<>))
import Data.Sequence (Seq)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import qualified Rank2
import Rank2 (apply)
import Reflex.Host.Class (ReflexHost(type EventHandle, type EventTrigger))
import Reflex.Native.TextStyle (TextStyle(..))
import Reflex.Native.ViewLayout (ViewLayout)
import Reflex.Native.ViewStyle (ViewStyle(..))
import Reflex.PerformEvent.Base (FireCommand)
import Reflex.Spider (SpiderHost, SpiderTimeline)
import Reflex.TriggerEvent.Base (EventTriggerRef, TriggerInvocation)
import System.IO.Unsafe (unsafePerformIO)


-- |A unique identity for a test view, holder, marker, or similar thing qualified by what it's an identity for. Almost identical to "Data.Unique" except that
-- this has a more useful 'Show' instance for diagnostics.
newtype TestIdentity = TestIdentity { unTestIdentity :: Integer } deriving (Eq, Ord)

-- |Show a unique identifier for diagnostics.
tshowTestIdentity :: TestIdentity -> Text
tshowTestIdentity (TestIdentity i) = pack ('#' : show i)

-- |Shared reference to make unique 'TestIdentity' values
{-# NOINLINE nextTestIdentityRef #-}
nextTestIdentityRef :: IORef Integer
nextTestIdentityRef = unsafePerformIO $ newIORef 1

-- |Create a new 'TestIdentity' with a new serial number
newTestIdentity :: MonadIO m => m TestIdentity
newTestIdentity =
  fmap TestIdentity . liftIO . atomicModifyIORef' nextTestIdentityRef $ \ n ->
    let n' = succ n in (n', n')

-- |'ShowS' for a @'ViewStyle' Identity@ since 'Show' is needed for test assertion messages and so on.
showsViewStyle :: ViewStyle Identity -> ShowS
showsViewStyle (ViewStyle {..})
  = ('{':)
  . showString "bg=" . shows (runIdentity _viewStyle_backgroundColor)
  . ('}':)

-- |Common attributes of every view in a test view hierarchy. Parameterized by @f@ which wraps every value; @f ~ TVar@ during the building step, and
-- @f ~ Identity@ for frozen copies of the view hierarchy.
data TestViewCommon v = TestViewCommon
  { _testViewCommon_identity :: TestIdentity
  -- ^Unique identity of the view for distinguising it among others.
  , _testViewCommon_style :: ViewStyle v
  -- ^The style of the view.
  , _testViewCommon_layout :: v ViewLayout
  -- ^The layout of the view.
  , _testViewCommon_accessibilityLabel :: v (Maybe Text)
  -- ^The accessibility label of the view.
  } deriving (Generic)

-- |Show a @TestViewCommon@ for test assertion messages and the like. Usually used as the first part of showing the view type embedding the @TestViewCommon@.
instance Show (TestViewCommon Identity) where
  showsPrec _ (TestViewCommon {..})
    = ('#':) . shows (unTestIdentity _testViewCommon_identity)
    . showString " style=" . showsViewStyle _testViewCommon_style
    . showString " layout=" . shows (runIdentity _testViewCommon_layout)
    . showString " accessibilityLabel=" . shows (runIdentity _testViewCommon_accessibilityLabel)

instance Rank2.Functor TestViewCommon where
  f <$> TestViewCommon a b c d = TestViewCommon a (f Rank2.<$> b) (f c) (f d)
instance Rank2.Apply TestViewCommon where
  TestViewCommon _ fb fc fd <*> TestViewCommon a b c d = TestViewCommon a (fb Rank2.<*> b) (apply fc c) (apply fd d)
instance Rank2.Applicative TestViewCommon where
  pure f = TestViewCommon (TestIdentity (-1) {- it's a hack! -}) (Rank2.pure f) f f
instance Rank2.Foldable TestViewCommon where
  foldMap f (TestViewCommon _ b c d) = Rank2.foldMap f b <> f c <> f d
instance Rank2.Traversable TestViewCommon where
  traverse f (TestViewCommon a b c d) = TestViewCommon a <$> Rank2.traverse f b <*> f c <*> f d

-- |A container view which has common view attributes and a collection of subviews.
data TestContainerView v = TestContainerView
  { _testContainerView_common :: TestViewCommon v
  -- ^The common view attributes for the container.
  , _testContainerView_contents :: v (Seq (TestView v))
  -- ^The subviews.
  } deriving (Generic)
-- can't instance rank-2 Functor on account of the fixed point - would need @v@ or @v'@ to be a Functor but they need to be natural.

-- |Show a 'TestContainerView' for test assertion messages and the like.
instance Show (TestContainerView Identity) where
  showsPrec _ = showsTestContainerView True

-- |Show a 'TestContainerView' for test assertion messages and the like. Takes a boolean indicating whether subviews will be dumped (@True@) or not (@False@).
showsTestContainerView :: Bool -> TestContainerView Identity -> ShowS
showsTestContainerView recurse (TestContainerView {..})
  = showString "container " . shows _testContainerView_common
  . (if recurse then (' ':) . showList (toList _testContainerView_contents) else id)

-- |Traverse some effect through a @'TestContainerView' v@ while changing @v -> v'@. See 'traverseTestView' for how this is commonly used.
-- The traversal effect needs to accept an additional mapping effect to apply inside in order to handle the fixed point @_testContainerView_contents@.
traverseTestContainerView
  :: Applicative f
  => (forall a b. (a -> f b) -> v a -> f (v' b))
  -> TestContainerView v -> f (TestContainerView v')
traverseTestContainerView f (TestContainerView {..}) =
  TestContainerView
    <$> Rank2.traverse (f pure) _testContainerView_common
    <*> f (traverse (traverseTestView f)) _testContainerView_contents

-- |Show the type and identity of a test container view, equivalent to @'tshowTestViewIdentity' . 'TestView_Container'@
tshowTestContainerViewIdentity :: TestContainerView v -> Text
tshowTestContainerViewIdentity = tshowTestViewIdentity . TestView_Container

-- |A text display view which has common view attributes, a text style, and whatever the current/captured text is.
data TestTextView v = TestTextView
  { _testTextView_common :: TestViewCommon v
  -- ^The common view attributes for the text view.
  , _testTextView_style :: TextStyle v
  -- ^The style to display the text with.
  , _testTextView_text :: v Text
  -- ^The actual text.
  } deriving (Generic)

-- |'ShowS' for a @'TextStyle' Identity@ since 'Show' is needed for test assertion messages and so on.
showsTextStyle :: TextStyle Identity -> ShowS
showsTextStyle (TextStyle {..})
  = showString "{color=" . shows (runIdentity _textStyle_textColor)
  . showString " font=" . shows (runIdentity _textStyle_font)
  . ('}':)

-- |Show a 'TestTextView' for test assertion messages and the like.
instance Show (TestTextView Identity) where
  showsPrec _ (TestTextView {..})
    = showString "text " . shows _testTextView_common
    . showString " textStyle=" . showsTextStyle _testTextView_style
    . showString " text=" . shows (runIdentity _testTextView_text)

instance Rank2.Functor TestTextView where
  f <$> TestTextView a b c = TestTextView (f Rank2.<$> a) (f Rank2.<$> b) (f c)
instance Rank2.Apply TestTextView where
  TestTextView fa fb fc <*> TestTextView a b c = TestTextView (fa Rank2.<*> a) (fb Rank2.<*> b) (apply fc c)
instance Rank2.Applicative TestTextView where
  pure f = TestTextView (Rank2.pure f) (Rank2.pure f) f
instance Rank2.Foldable TestTextView where
  foldMap f (TestTextView a b c) = Rank2.foldMap f a <> Rank2.foldMap f b <> f c
instance Rank2.Traversable TestTextView where
  traverse f (TestTextView a b c) = TestTextView <$> Rank2.traverse f a <*> Rank2.traverse f b <*> f c

-- |Show the type and identity of a test text view, equivalent to @'tshowTestViewIdentity' . 'TestView_Text'@
tshowTestTextViewIdentity :: TestTextView v -> Text
tshowTestTextViewIdentity = tshowTestViewIdentity . TestView_Text

-- |A marker view node which doesn't have any display but denotes the boundary between replaceable view segments.
data TestMarker = TestMarker
  { _testMarker_identity :: TestIdentity
  -- ^The unique identity of the marker.
  , _testMarker_parent :: TVar (Maybe (TVar (Seq (TestView TVar))))
  -- ^Where the marker is installed, or Nothing if it's not installed.
  } deriving (Eq, Generic)

-- |Show a 'TestMarker' for test assertion messages and the like.
instance Show TestMarker where
  showsPrec _ (TestMarker {..}) = showString "marker #" . shows (unTestIdentity _testMarker_identity)

-- |Show the type and identity of a test marker, equivalent to @'tshowTestViewIdentity' . 'TestView_Marker'@
tshowTestMarkerIdentity :: TestMarker -> Text
tshowTestMarkerIdentity = tshowTestViewIdentity . TestView_Marker

-- |A node in the view hierarchy, either one of the @Test*View@ types or a special marker used during build time to isolate sections of the subviews.
data TestView v
  = TestView_Container (TestContainerView v)
  | TestView_Text (TestTextView v)
  | TestView_Marker TestMarker
  deriving (Generic)

-- |Show a 'TestView' for test assertion messages and the like.
instance Show (TestView Identity) where
  showsPrec _ = showsTestView True

-- |Show a 'TestView' for test assertion messages and the like. Takes a boolean which controls whether subviews will be dumped (@True@) or not (@False).
showsTestView :: Bool -> TestView Identity -> ShowS
showsTestView recurse = \ case
  -- each of the view types includes show output indicating their type, so don't duplicate it here
  TestView_Container cv -> showsTestContainerView recurse cv
  TestView_Text tv -> shows tv
  TestView_Marker m -> shows m

-- |Show a 'TestView' hierarchy on multiple lines with indenting.
showTestViewHierarchy :: String -> Seq (TestView Identity) -> [String]
showTestViewHierarchy prefix = DList.toList . go prefix
  where
    go :: String -> Seq (TestView Identity) -> DList String
    go indent = foldMap (visit indent) . toList

    visit :: String -> TestView Identity -> DList String
    visit indent = \ case
      TestView_Container cv ->
        DList.cons
          (indent ++ showsTestContainerView False cv "")
          (go (' ':' ':indent) . runIdentity . _testContainerView_contents $ cv)
      other -> DList.singleton $ indent ++ showsTestView False other ""

-- |Test for equal identity of two view nodes
instance Eq (TestView v) where
  a == b = _testView_identity a == _testView_identity b

-- |Traverse some effect through a @'TestView' v@ while changing @v -> v'@. This is used to do any recursive effect on a view hierarchy, such as freezing a
-- @'TestView' TVar@ into a @'TestView' Identity@ via @atomically . traverseTestView (\ f -> pure . Identity <=< f <=< readTVar)@.
-- The traversal effect needs to accept an additional mapping effect to apply inside in order to handle the fixed point @_testContainerView_contents@ if the
-- view is a @TestView_Container@.
traverseTestView
  :: Applicative f
  => (forall a b. (a -> f b) -> v a -> f (v' b))
  -> TestView v -> f (TestView v')
traverseTestView f = \ case
  TestView_Container cv -> TestView_Container <$> traverseTestContainerView f cv
  TestView_Text tv -> TestView_Text <$> Rank2.traverse (f pure) tv
  TestView_Marker m -> pure (TestView_Marker m)

-- |Show the type and identity of a view node
tshowTestViewIdentity :: TestView v -> Text
tshowTestViewIdentity = \ case
  TestView_Container cv -> "container " <> tshowTestIdentity (_testViewCommon_identity . _testContainerView_common $ cv)
  TestView_Text tv -> "text " <> tshowTestIdentity (_testViewCommon_identity . _testTextView_common $ tv)
  TestView_Marker m -> "marker " <> tshowTestIdentity (_testMarker_identity m)

-- |Project the 'TestViewCommon' out of a 'TestView', if it's not a 'TestView_Marker'.
_testView_common :: TestView v -> Maybe (TestViewCommon v)
_testView_common = \ case
  TestView_Container cv -> Just . _testContainerView_common $ cv
  TestView_Text tv -> Just . _testTextView_common $ tv
  TestView_Marker _ -> Nothing

-- |Project the unique identity out of a 'TestView'
_testView_identity :: TestView v -> TestIdentity
_testView_identity = \ case
  TestView_Container cv -> _testViewCommon_identity . _testContainerView_common $ cv
  TestView_Text tv -> _testViewCommon_identity . _testTextView_common $ tv
  TestView_Marker m -> _testMarker_identity m

-- |Type which holds a sequence of views. The same type as @_testContainerView_contents@ for @'TestContainerView' TVar@
type TestHolder = TVar (Seq (TestView TVar))

-- |The environment of an in-progress test with a handle to the view hierarchy and the event processing channel.
data TestEnv x = TestEnv
  { _testEnv_rootHolder :: TestHolder
  -- ^The root of the view hierarchy.
  , _testEnv_rootReady :: TVar Bool
  -- ^True iff the first build was immediately ready or it's been committed since.
  , _testEnv_eventChan :: Chan [DSum (EventTriggerRef (SpiderTimeline x)) TriggerInvocation]
  -- ^The event channel to write new event trigger invocations to.
  , _testEnv_fireCommand :: FireCommand (SpiderTimeline x) (SpiderHost x)
  -- ^The 'FireCommand' which is used to process events with the underlying host and then perform any actions triggered by those events.
  , _testEnv_stepCompleteEventHandle :: EventHandle (SpiderTimeline x) ()
  -- ^The event which is fired after each test evaluation step to ensure that event processing has been finished. This is especially required since @Chan@s can
  -- only be read by blocking, so we need an event to explicitly bookend the step.
  , _testEnv_stepCompleteTriggerRef :: IORef (Maybe (EventTrigger (SpiderTimeline x) ()))
  -- ^The trigger for @_testEnv_stepCompleteEvent@.
  }

-- |The monad for evaluating an in-progress test after the build has completed and has access to the state of the view hierarchy and event processing channel.
newtype TestEvaluation x a = TestEvaluation { unTestEvaluation :: RWST (TestEnv x) () (Seq (TestView Identity)) (SpiderHost x) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException)
