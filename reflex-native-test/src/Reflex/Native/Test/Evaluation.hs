{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- |Functions for evaluating the current state of a built UI hierarchy and making assertions about that state.
module Reflex.Native.Test.Evaluation
  (
  -- * Basic functions
    askRootViews, askRootReady, runFrame, selectFromViews, lookupFromViews
  -- * "Test.Hspec.Expectations" lifted to 'TestEvaluation'
  , expectationFailure, shouldBe, shouldSatisfy, shouldStartWith, shouldEndWith, shouldContain, shouldMatchList, shouldReturn, shouldNotBe, shouldNotSatisfy
  , shouldNotContain, shouldNotReturn
  -- * Expectations similar to @Test.Hspec.Expectations.Lens@ for views
  , shouldHave, shouldNotHave, shouldView, shouldPreview, shouldList
  ) where

import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Lens (Getting, has, hasn't, preview, toListOf, view)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS.Strict (asks, get, gets)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity)
import Data.List (intercalate)
import Data.Monoid (All, Any, Endo, First)
import Data.Sequence (Seq)
import GHC.Stack (HasCallStack)
import Reflex.Host.Class (ReflexHost(type HostFrame), runHostFrame)
import Reflex.Native.Test.Types (TestEnv(..), TestEvaluation(..), TestView, showTestViewHierarchy)
import Reflex.Spider.Internal (HasSpiderTimeline, SpiderTimeline)
import Reflex.TriggerEvent.Base (TriggerEventT, runTriggerEventT)
import qualified Test.Hspec.Expectations as Hspec


-- |Get the current view hierarchy, updated each time 'processEventsAndRead' is run.
askRootViews :: TestEvaluation x (Seq (TestView Identity))
askRootViews = TestEvaluation get

-- |Get whether the root has become ready.
askRootReady :: TestEvaluation x Bool
askRootReady = TestEvaluation (asks _testEnv_rootReady) >>= liftIO . readTVarIO

-- |Run a frame of Reflex, letting you sample, hold, subscribe to events, or create triggers during a 'TestEvaluation'.
runFrame
  :: forall x a. HasSpiderTimeline x
  => (TriggerEventT (SpiderTimeline x) (HostFrame (SpiderTimeline x)) a)
  -> TestEvaluation x a
runFrame action = TestEvaluation $ do
  chan <- asks _testEnv_eventChan
  lift $ runHostFrame (runTriggerEventT action chan)

-- |Apply some @Fold@ to the current view hierarchy returning the results.
selectFromViews :: Getting (Endo [a]) (Seq (TestView Identity)) a -> TestEvaluation x [a]
selectFromViews f = TestEvaluation $ gets (toListOf f)

-- |Apply some @Fold@ to the current view hierarchy returning the first result if any.
lookupFromViews :: Getting (First a) (Seq (TestView Identity)) a -> TestEvaluation x (Maybe a)
lookupFromViews f = TestEvaluation $ gets (preview f)

-- |Signal that an expectation failed with some message, aborting the test.
expectationFailure :: HasCallStack => String -> TestEvaluation x ()
expectationFailure = liftIO . Hspec.expectationFailure

infix 2 `shouldBe`, `shouldSatisfy`, `shouldStartWith`, `shouldEndWith`, `shouldContain`, `shouldMatchList`, `shouldReturn`
infix 2 `shouldNotBe`, `shouldNotSatisfy`, `shouldNotContain`, `shouldNotReturn`

-- |@actual \`shouldBe\` expected@ sets the expectation that @actual@ is equal to @expected@.
shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> TestEvaluation x ()
actual `shouldBe` expected = liftIO $ actual `Hspec.shouldBe` expected

-- |@v \`shouldSatisfy\` p@ sets the expectation that @p v@ is @True@.
shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> TestEvaluation x ()
v `shouldSatisfy` p = liftIO $ v `Hspec.shouldSatisfy` p

-- |@list \`shouldStartWith\` prefix@ sets the expectation that @list@ starts with @prefix@,
shouldStartWith :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> TestEvaluation x ()
xs `shouldStartWith` ys = liftIO $ xs `Hspec.shouldStartWith` ys

-- |@list \`shouldEndWith\` suffix@ sets the expectation that @list@ ends with @suffix@,
shouldEndWith :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> TestEvaluation x ()
xs `shouldEndWith` ys = liftIO $ xs `Hspec.shouldEndWith` ys

-- |@list \`shouldContain\` sublist@ sets the expectation that @sublist@ is contained, wholly and intact, anywhere in @list@.
shouldContain :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> TestEvaluation x ()
xs `shouldContain` ys = liftIO $ xs `Hspec.shouldContain` ys

-- |@xs \`shouldMatchList\` ys@ sets the expectation that @xs@ has the same elements that @ys@ has, possibly in another order
shouldMatchList :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> TestEvaluation x ()
xs `shouldMatchList` ys = liftIO $ xs `Hspec.shouldMatchList` ys

-- |@action \`shouldReturn\` expected@ sets the expectation that @action@ returns @expected@.
shouldReturn :: (HasCallStack, Show a, Eq a) => TestEvaluation x a -> a -> TestEvaluation x ()
action `shouldReturn` expected = action >>= (`shouldBe` expected)

-- |@actual \`shouldNotBe\` notExpected@ sets the expectation that @actual@ is not equal to @notExpected@
shouldNotBe :: (HasCallStack, Show a, Eq a) => a -> a -> TestEvaluation x ()
actual `shouldNotBe` notExpected = liftIO $ actual `Hspec.shouldNotBe` notExpected

-- |@v \`shouldNotSatisfy\` p@ sets the expectation that @p v@ is @False@.
shouldNotSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> TestEvaluation x ()
v `shouldNotSatisfy` p = liftIO $ v `Hspec.shouldNotSatisfy` p

-- |@list \`shouldNotContain\` sublist@ sets the expectation that @sublist@ is not contained anywhere in @list@.
shouldNotContain :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> TestEvaluation x ()
list `shouldNotContain` sublist = liftIO $ list `Hspec.shouldNotContain` sublist

-- |@action \`shouldNotReturn\` notExpected@ sets the expectation that @action@ does not return @notExpected@.
shouldNotReturn :: (HasCallStack, Show a, Eq a) => TestEvaluation x a -> a -> TestEvaluation x ()
action `shouldNotReturn` notExpected = action >>= (`shouldNotBe` notExpected)

infixl 2 `shouldHave`, `shouldNotHave`, `shouldView`, `shouldPreview`, `shouldList`

-- | @s \`shouldHave\` l@ sets the expectation that 'Fold' @l@ has non-zero number of targets in the view hierarchy
--
-- > s `shouldBe` t ≡ s `shouldHave` only t
--
-- @
-- shouldHave :: 'Getter'     (Seq ('TestView' Identity)) a -> 'TestEvaluation' x ()
-- shouldHave :: 'Fold'       (Seq ('TestView' Identity)) a -> 'TestEvaluation' x ()
-- shouldHave :: 'Iso''       (Seq ('TestView' Identity)) a -> 'TestEvaluation' x ()
-- shouldHave :: 'Lens''      (Seq ('TestView' Identity)) a -> 'TestEvaluation' x ()
-- shouldHave :: 'Traversal'' (Seq ('TestView' Identity)) a -> 'TestEvaluation' x ()
-- shouldHave :: 'Prism''     (Seq ('TestView' Identity)) a -> 'TestEvaluation' x ()
-- @
shouldHave :: HasCallStack => Getting Any (Seq (TestView Identity)) a -> TestEvaluation x ()
shouldHave l = do
  vs <- askRootViews
  unless (has l vs) $
    expectationFailure $ "Fold had zero targets but expected at least one in:\n" ++ intercalate "\n" (showTestViewHierarchy "  " vs)

-- | @shouldNotHave l@ sets the expectation that 'Fold' @l@ has exactly zero targets in the view hierarchy
--
-- @
-- shouldNotHave :: 'Getter'     (Seq ('TestView' Identity)) a -> 'TestEvaluation' x ()
-- shouldNotHave :: 'Fold'       (Seq ('TestView' Identity)) a -> 'TestEvaluation' x ()
-- shouldNotHave :: 'Iso''       (Seq ('TestView' Identity)) a -> 'TestEvaluation' x ()
-- shouldNotHave :: 'Lens''      (Seq ('TestView' Identity)) a -> 'TestEvaluation' x ()
-- shouldNotHave :: 'Traversal'' (Seq ('TestView' Identity)) a -> 'TestEvaluation' x ()
-- shouldNotHave :: 'Prism''     (Seq ('TestView' Identity)) a -> 'TestEvaluation' x ()
-- @
shouldNotHave :: (HasCallStack, Show a) => Getting All (Seq (TestView Identity)) a -> TestEvaluation x ()
shouldNotHave l = do
  vs <- askRootViews
  unless (hasn't l vs) $ do
    expectationFailure $ "Fold was supposed to have zero targets in:\n" ++ intercalate "\n" (showTestViewHierarchy "  " vs)

-- | @l \`shouldView\` t@ sets the expectation that you can see target @t@ in the view hierarchy though a 'Getter' @l@
--
-- @
-- shouldView ::           ('Show' a, 'Eq' a) => 'Getter'     (Seq ('TestView' Identity)) a -> a -> 'TestEvaluation' x ()
-- shouldView :: ('Data.Monoid.Monoid' m, 'Show' a, 'Eq' a) => 'Fold'       (Seq ('TestView' Identity)) m -> a -> 'TestEvaluation' x ()
-- shouldView ::           ('Show' a, 'Eq' a) => 'Iso''       (Seq ('TestView' Identity)) a -> a -> 'TestEvaluation' x ()
-- shouldView ::           ('Show' a, 'Eq' a) => 'Lens''      (Seq ('TestView' Identity)) a -> a -> 'TestEvaluation' x ()
-- shouldView :: ('Data.Monoid.Monoid' m, 'Show' a, 'Eq' a) => 'Traversal'' (Seq ('TestView' Identity)) m -> a -> 'TestEvaluation' x ()
-- shouldView :: ('Data.Monoid.Monoid' m, 'Show' a, 'Eq' a) => 'Prism''     (Seq ('TestView' Identity)) m -> a -> 'TestEvaluation' x ()
-- @
shouldView :: (HasCallStack, Show a, Eq a) => Getting a (Seq (TestView Identity)) a -> a -> TestEvaluation x ()
l `shouldView` t = do
  vs <- askRootViews
  let t' = view l vs
  when (t /= t') $
    expectationFailure $ "expected " ++ show t ++ " but got " ++ show t' ++ " in:\n" ++ intercalate "\n" (showTestViewHierarchy "  " vs)

-- | @l \`shouldPreview\` t@ sets the expectation that your @y@ is the first target of the 'Fold' @l@ in the view hierarchy
--
-- @
-- shouldPreview :: ('Show' a, 'Eq' a) => 'Getter'     (Seq ('TestView' Identity)) a -> a -> 'TestEvaluation' x ()
-- shouldPreview :: ('Show' a, 'Eq' a) => 'Fold'       (Seq ('TestView' Identity)) a -> a -> 'TestEvaluation' x ()
-- shouldPreview :: ('Show' a, 'Eq' a) => 'Lens''      (Seq ('TestView' Identity)) a -> a -> 'TestEvaluation' x ()
-- shouldPreview :: ('Show' a, 'Eq' a) => 'Iso''       (Seq ('TestView' Identity)) a -> a -> 'TestEvaluation' x ()
-- shouldPreview :: ('Show' a, 'Eq' a) => 'Traversal'' (Seq ('TestView' Identity)) a -> a -> 'TestEvaluation' x ()
-- shouldPreview :: ('Show' a, 'Eq' a) => 'Prism''     (Seq ('TestView' Identity)) a -> a -> 'TestEvaluation' x ()
-- @
shouldPreview :: (HasCallStack, Show a, Eq a) => Getting (First a) (Seq (TestView Identity)) a -> a -> TestEvaluation x ()
l `shouldPreview` t = do
  vs <- askRootViews
  let t'May = preview l vs
  when (Just t /= t'May) $
    expectationFailure $ "expected (Just) " ++ show t ++ " but got " ++ show t'May ++ " in:\n" ++ intercalate "\n" (showTestViewHierarchy "  " vs)

-- | @l \`shouldList\` ts@ sets the expectation that @ts@ is a list of the Fold @l@ targets in the view hierarchy.
--
-- @
-- shouldList :: ('Show' a, 'Eq' a) => s -> [a] -> 'Getter'     s a -> 'TestEvaluation' x ()
-- shouldList :: ('Show' a, 'Eq' a) => s -> [a] -> 'Fold'       s a -> 'TestEvaluation' x ()
-- shouldList :: ('Show' a, 'Eq' a) => s -> [a] -> 'Lens''      s a -> 'TestEvaluation' x ()
-- shouldList :: ('Show' a, 'Eq' a) => s -> [a] -> 'Iso''       s a -> 'TestEvaluation' x ()
-- shouldList :: ('Show' a, 'Eq' a) => s -> [a] -> 'Traversal'' s a -> 'TestEvaluation' x ()
-- shouldList :: ('Show' a, 'Eq' a) => s -> [a] -> 'Prism''     s a -> 'TestEvaluation' x ()
-- @
shouldList :: (HasCallStack, Show a, Eq a) => Getting (Endo [a]) (Seq (TestView Identity)) a -> [a] -> TestEvaluation x ()
l `shouldList` ts = do
  vs <- askRootViews
  let ts' = toListOf l vs
  when (ts /= ts') $
    expectationFailure $ "expected " ++ show ts ++ " but got " ++ show ts' ++ " in:\n" ++ intercalate "\n" (showTestViewHierarchy "  " vs)
