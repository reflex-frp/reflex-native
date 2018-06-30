{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Lens (Getting, cosmosOf, each)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum((:=>)))
import Data.Foldable (for_)
import Data.Functor (($>), void)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Misc (Const2(..))
import qualified Data.IntMap as IntMap
import Data.Monoid (Endo, (<>))
import Data.Sequence (Seq)
import Data.Text (Text)
import Reflex
  ( Event, PatchIntMap(..), newTriggerEvent, runWithReplace, traverseIntMapWithKeyWithAdjust, traverseDMapWithKeyWithAdjustWithMove )
import Reflex.Native (container_, text_, notReady, notReadyUntil)
import Reflex.Native.Test
  ( withTestHost, testWith, processEvents
  , TestEvaluation, askRootReady, runFrame, shouldList, shouldReturn, selectFromViews
  , TestView, _Marker, _Text, text_text, subviews
  )
import Reflex.Patch.DMapWithMove (deleteDMapKey, insertDMapKey, moveDMapKey, swapDMapKey)
import Test.Hspec (Spec, context, describe, hspec, specify)


countMarkers :: TestEvaluation x Int
countMarkers = length <$> selectFromViews (each . _Marker)

allTexts :: Getting (Endo [Text]) (Seq (TestView Identity)) Text
allTexts = each . cosmosOf (subviews . each) . _Text . text_text

main :: IO ()
main = hspec $ do
  describe "TestViewBuilder and AdjustingBuilder" $ do
    specify "simple build" $ do
      withTestHost $ \ _ -> testWith (container_ (text_ "text")) (\ _ -> askRootReady `shouldReturn` True >> allTexts `shouldList` ["text"])

    context "runWithReplace" runWithReplaceTests
    context "traverseIntMapWithKeyWithAdjust" traverseIntMapWithKeyWithAdjustTests
    context "traverseDMapWithKeyWithAdjustWithMove" traverseDMapWithKeyWithAdjustWithMoveTests

runWithReplaceTests :: Spec
runWithReplaceTests = do
  specify "simple" $ do
    withTestHost $ \ _ -> testWith
      ( do
          (replaceEvent, triggerReplace) <- newTriggerEvent
          void $ runWithReplace (text_ "first") (text_ <$> replaceEvent)
          pure triggerReplace
      )
      ( \ triggerReplace -> do
          countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["first"]

          liftIO $ triggerReplace "second"
          processEvents >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["second"]
      )

  specify "notReady" $ do
    withTestHost $ \ _ -> testWith
      ( do
          (replaceEvent, triggerReplace) <- newTriggerEvent
          void $ runWithReplace (text_ "first" >> notReady) replaceEvent
          pure triggerReplace
      )
      ( \ triggerReplace -> do
          -- first generation is never ready, so shouldn't be in the hierarchy though the markers should be
          askRootReady `shouldReturn` False >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` []

          -- replacing the not ready generation should work just as it would if the first generation was ready
          liftIO . triggerReplace $ text_ "second"
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["second"]

          -- replacing a ready generation with a not ready one shouldn't change the hierarchy
          liftIO . triggerReplace $ text_ "third" >> notReady
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["second"]

          -- and just make sure replacing a non-first not ready generation works correctly
          liftIO . triggerReplace $ text_ "fourth"
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["fourth"]
      )

  specify "notReadyUntil" $ do
    withTestHost $ \ _ -> testWith
      ( do
          (replaceEvent, triggerReplace) <- newTriggerEvent
          (readyEvent, triggerReady) <- newTriggerEvent
          void $ runWithReplace (text_ "first" >> notReadyUntil readyEvent) replaceEvent
          pure (triggerReplace, triggerReady)
      )
      ( \ (triggerReplace, triggerReady) -> do
          -- nothing from the first generation should be there except the markers since it's not ready yet
          askRootReady `shouldReturn` False >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` []

          -- triggering the ready event should cause the view to be installed
          liftIO . triggerReady $ ()
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["first"]

          -- triggering it again shouldn't do anything
          liftIO . triggerReady $ ()
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["first"]

          -- replacing with another not ready generation should leave the first generation in place
          (readyEvent2, triggerReady2) <- runFrame newTriggerEvent
          liftIO . triggerReplace $ text_ "second" >> notReadyUntil readyEvent2
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["first"]

          -- replacing with another not ready generation should still leave the first generation
          (readyEvent3, triggerReady3) <- runFrame newTriggerEvent
          liftIO . triggerReplace $ text_ "third" >> notReadyUntil readyEvent3
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["first"]

          -- triggering ready on the third generation should cause it to replace the first
          liftIO . triggerReady3 $ ()
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["third"]

          -- but triggering ready on the replaced second generation should do nothing
          liftIO . triggerReady2 $ ()
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["third"]

          -- installing two new not ready generations should be as if installing only the second
          (readyEvent4, triggerReady4) <- runFrame newTriggerEvent
          (readyEvent5, triggerReady5) <- runFrame newTriggerEvent
          liftIO . triggerReplace $ text_ "fourth" >> notReadyUntil readyEvent4
          liftIO . triggerReplace $ text_ "fifth" >> notReadyUntil readyEvent5
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["third"]
          liftIO . triggerReady4 $ ()
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["third"] -- because 5 replaced 4
          liftIO . triggerReady5 $ ()
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["fifth"]
      )

traverseIntMapWithKeyWithAdjustTests :: Spec
traverseIntMapWithKeyWithAdjustTests = do
  specify "inserts and deletes" $ do
    withTestHost $ \ _ -> testWith
      ( do
          (replaceEvent, triggerReplace) <- newTriggerEvent
          void $ traverseIntMapWithKeyWithAdjust (\ _ -> text_) (IntMap.singleton 1 "one") replaceEvent
          pure triggerReplace
      )
      ( \ triggerReplace -> do
          countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["one"]

          liftIO . triggerReplace . PatchIntMap $ IntMap.singleton 2 (Just "two")
          processEvents >> countMarkers `shouldReturn` 3 >> allTexts `shouldList` ["one", "two"]

          liftIO . triggerReplace . PatchIntMap $ IntMap.singleton 0 (Just "zero")
          processEvents >> countMarkers `shouldReturn` 4 >> allTexts `shouldList` ["zero", "one", "two"]

          liftIO . triggerReplace . PatchIntMap $ IntMap.singleton 1 Nothing
          processEvents >> countMarkers `shouldReturn` 3 >> allTexts `shouldList` ["zero", "two"]

          liftIO . triggerReplace . PatchIntMap $ IntMap.singleton 0 Nothing
          processEvents >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["two"]

          liftIO . triggerReplace . PatchIntMap $ IntMap.singleton 2 Nothing
          processEvents >> countMarkers `shouldReturn` 1 >> allTexts `shouldList` []

          liftIO . triggerReplace . PatchIntMap $ IntMap.singleton 2 (Just "two 2")
          processEvents >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["two 2"]
      )

  specify "delete never ready" $ do
    withTestHost $ \ _ -> testWith
      ( do
          (replaceEvent, triggerReplace) <- newTriggerEvent
          void $ traverseIntMapWithKeyWithAdjust
            (\ _ (t, nr) -> text_ t >> when nr notReady)
            (IntMap.fromList [(1, ("one", False)), (2, ("two", True))])
            replaceEvent
          pure triggerReplace
      )
      ( \ triggerReplace -> do
          askRootReady `shouldReturn` False >> countMarkers `shouldReturn` 3 >> allTexts `shouldList` ["one"]

          liftIO . triggerReplace . PatchIntMap $ IntMap.singleton 2 Nothing
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["one"]
      )

  specify "never ready slots" $ do
    withTestHost $ \ _ -> testWith
      ( do
          (replaceEvent, triggerReplace) <- newTriggerEvent
          (readyEvent, triggerReady) <- newTriggerEvent
          void $ traverseIntMapWithKeyWithAdjust
            (\ _ (t, eMay) -> text_ t >> for_ eMay notReadyUntil)
            (IntMap.fromList [(1, ("one", Just readyEvent)), (2, ("two", Nothing))])
            replaceEvent
          pure (triggerReplace, triggerReady)
      )
      ( \ (triggerReplace, triggerReady) -> do
          askRootReady `shouldReturn` False >> countMarkers `shouldReturn` 3 >> allTexts `shouldList` ["two"]

          -- replacing the second slot which was initially ready with a new one that isn't should keep the first generation's view
          (readyEvent2, triggerReady2) <- runFrame newTriggerEvent
          liftIO . triggerReplace . PatchIntMap $ IntMap.singleton 2 (Just ("two 2", Just readyEvent2))
          processEvents >> askRootReady `shouldReturn` False >> countMarkers `shouldReturn` 3 >> allTexts `shouldList` ["two"]

          -- making the first slot ready should cause the root to become ready because the second slot was ready at first and has a valid view in it
          liftIO . triggerReady $ ()
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 3 >> allTexts `shouldList` ["one", "two"]

          -- now that the first slot is ready, deleting the second slot should now make the parent ready
          liftIO . triggerReplace . PatchIntMap $ IntMap.singleton 2 Nothing
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["one"]

          -- and marking the now deleted second slot should do nothing
          liftIO . triggerReady2 $ ()
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["one"]
      )

traverseDMapWithKeyWithAdjustWithMoveTests :: Spec
traverseDMapWithKeyWithAdjustWithMoveTests = do
  specify "swap" $ do
    withTestHost $ \ _ -> testWith
      ( do
          (replaceEvent, triggerReplace) <- newTriggerEvent
          let dm :: DMap (Const2 Int Text) Identity
              dm = DMap.fromList [Const2 1 :=> Identity "one", Const2 2 :=> "two"]
          (_ :: DMap (Const2 Int Text) (Const ()), _) <- traverseDMapWithKeyWithAdjustWithMove
            (\ (Const2 _) (Identity t) -> text_ t $> Const ())
            dm
            replaceEvent
          pure triggerReplace
      )
      ( \ triggerReplace -> do
          countMarkers `shouldReturn` 3 >> allTexts `shouldList` ["one", "two"]

          liftIO . triggerReplace $ swapDMapKey (Const2 1) (Const2 2)
          processEvents >> allTexts `shouldList` ["two", "one"]

          liftIO . triggerReplace $ swapDMapKey (Const2 1) (Const2 2)
          processEvents >> allTexts `shouldList` ["one", "two"]
      )

  specify "move with notReadyUntil" $ do
    withTestHost $ \ (_ :: proxy t) -> testWith
      ( do
          (replaceEvent, triggerReplace) <- newTriggerEvent
          (readyBEvent, triggerReadyB) <- newTriggerEvent
          let dm :: DMap (Const2 Int (Text, Maybe (Event t ()))) Identity
              dm = DMap.fromList [Const2 1 :=> Identity ("a", Nothing), Const2 2 :=> Identity ("b", Just readyBEvent)]
          (_ :: DMap (Const2 Int (Text, Maybe (Event t ()))) (Const ()), _) <- traverseDMapWithKeyWithAdjustWithMove
            (\ (Const2 _) (Identity (t, eMay)) -> for_ eMay notReadyUntil >> text_ t >> pure (Const ()))
            dm
            replaceEvent
          pure (triggerReplace, triggerReadyB)
      )
      ( \ (triggerReplace, triggerReadyB) -> do
          askRootReady `shouldReturn` False >> countMarkers `shouldReturn` 3 >> allTexts `shouldList` ["a"]

          -- commit handlers should follow key moves around, so if we move the unready 2 :=> "b" to 3 :=> "b" and then add in 2 :=> "c", readying b should not
          -- cause c to be installed, but instead correctly cause b to be inserted
          liftIO . triggerReplace $ insertDMapKey (Const2 2) (Identity ("c", Nothing)) <> moveDMapKey (Const2 2) (Const2 3)
          liftIO . triggerReadyB $ ()
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 4 >> allTexts `shouldList` ["a", "c", "b"]
      )

  specify "double move with notReadyUntil" $ do
    withTestHost $ \ (_ :: proxy t) -> testWith
      ( do
          (replaceEvent, triggerReplace) <- newTriggerEvent
          (readyBEvent, triggerReadyB) <- newTriggerEvent
          let dm :: DMap (Const2 Int (Text, Maybe (Event t ()))) Identity
              dm = DMap.fromList [Const2 1 :=> Identity ("a", Nothing), Const2 2 :=> Identity ("b", Just readyBEvent)]
          (_ :: DMap (Const2 Int (Text, Maybe (Event t ()))) (Const ()), _) <- traverseDMapWithKeyWithAdjustWithMove
            (\ (Const2 _) (Identity (t, eMay)) -> for_ eMay notReadyUntil >> text_ t >> pure (Const ()))
            dm
            replaceEvent
          pure (triggerReplace, triggerReadyB)
      )
      ( \ (triggerReplace, triggerReadyB) -> do
          askRootReady `shouldReturn` False >> countMarkers `shouldReturn` 3 >> allTexts `shouldList` ["a"]

          liftIO . triggerReplace $ moveDMapKey (Const2 2) (Const2 3)
          processEvents >> askRootReady `shouldReturn` False >> countMarkers `shouldReturn` 3 >> allTexts `shouldList` ["a"]

          liftIO . triggerReplace $ moveDMapKey (Const2 3) (Const2 4)
          processEvents >> askRootReady `shouldReturn` False >> countMarkers `shouldReturn` 3 >> allTexts `shouldList` ["a"]

          liftIO . triggerReadyB $ ()
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 3 >> allTexts `shouldList` ["a", "b"]
      )

  specify "move unready over ready" $ do
    withTestHost $ \ (_ :: proxy t) -> testWith
      ( do
          (replaceEvent, triggerReplace) <- newTriggerEvent
          (readyBEvent, triggerReadyB) <- newTriggerEvent
          let dm :: DMap (Const2 Int (Text, Maybe (Event t ()))) Identity
              dm = DMap.fromList [Const2 1 :=> Identity ("a", Nothing), Const2 2 :=> Identity ("b", Just readyBEvent)]
          (_ :: DMap (Const2 Int (Text, Maybe (Event t ()))) (Const ()), _) <- traverseDMapWithKeyWithAdjustWithMove
            (\ (Const2 _) (Identity (t, eMay)) -> for_ eMay notReadyUntil >> text_ t >> pure (Const ()))
            dm
            replaceEvent
          pure (triggerReplace, triggerReadyB)
      )
      ( \ (triggerReplace, triggerReadyB) -> do
          askRootReady `shouldReturn` False >> countMarkers `shouldReturn` 3 >> allTexts `shouldList` ["a"]

          liftIO . triggerReplace $ moveDMapKey (Const2 2) (Const2 1)
          processEvents >> askRootReady `shouldReturn` False >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` []

          liftIO . triggerReadyB $ ()
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["b"]
      )

  specify "move ready over unready" $ do
    withTestHost $ \ (_ :: proxy t) -> testWith
      ( do
          (replaceEvent, triggerReplace) <- newTriggerEvent
          (readyBEvent, triggerReadyB) <- newTriggerEvent
          let dm :: DMap (Const2 Int (Text, Maybe (Event t ()))) Identity
              dm = DMap.fromList [Const2 1 :=> Identity ("a", Nothing), Const2 2 :=> Identity ("b", Just readyBEvent)]
          (_ :: DMap (Const2 Int (Text, Maybe (Event t ()))) (Const ()), _) <- traverseDMapWithKeyWithAdjustWithMove
            (\ (Const2 _) (Identity (t, eMay)) -> for_ eMay notReadyUntil >> text_ t >> pure (Const ()))
            dm
            replaceEvent
          pure (triggerReplace, triggerReadyB)
      )
      ( \ (triggerReplace, triggerReadyB) -> do
          askRootReady `shouldReturn` False >> countMarkers `shouldReturn` 3 >> allTexts `shouldList` ["a"]

          liftIO . triggerReplace $ moveDMapKey (Const2 1) (Const2 2)
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["a"]

          liftIO . triggerReadyB $ ()
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["a"]
      )

  specify "insert ready after deleting unready" $ do
    withTestHost $ \ (_ :: proxy t) -> testWith
      ( do
          (replaceEvent, triggerReplace) <- newTriggerEvent
          (readyAEvent, triggerReadyA) <- newTriggerEvent
          let dm :: DMap (Const2 Int (Text, Maybe (Event t ()))) Identity
              dm = DMap.fromList [Const2 1 :=> Identity ("a", Just readyAEvent)]
          (_ :: DMap (Const2 Int (Text, Maybe (Event t ()))) (Const ()), _) <- traverseDMapWithKeyWithAdjustWithMove
            (\ (Const2 _) (Identity (t, eMay)) -> for_ eMay notReadyUntil >> text_ t >> pure (Const ()))
            dm
            replaceEvent
          pure (triggerReplace, triggerReadyA)
      )
      ( \ (triggerReplace, triggerReadyA) -> do
          askRootReady `shouldReturn` False >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` []

          liftIO . triggerReplace $ deleteDMapKey (Const2 1)
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 1 >> allTexts `shouldList` []

          liftIO . triggerReplace $ insertDMapKey (Const2 1) (Identity ("b", Nothing))
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["b"]

          liftIO . triggerReadyA $ ()
          processEvents >> askRootReady `shouldReturn` True >> countMarkers `shouldReturn` 2 >> allTexts `shouldList` ["b"]
      )

