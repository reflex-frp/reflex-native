{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |Specializations of various performance critical functions used in Reflex UIKit.
module Reflex.UIKit.Specializations where

import Data.Dependent.Map (DMap)
import Data.GADT.Compare (GCompare)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Reflex.Class (Event)
import Reflex.Patch.DMap (PatchDMap)
import Reflex.Patch.DMapWithMove (PatchDMapWithMove)
import Reflex.Patch.IntMap (PatchIntMap)
import Reflex.PerformEvent.Base (PerformEventT)
import Reflex.PostBuild.Base (PostBuildT, mapIntMapWithAdjustImpl)
import Reflex.Spider (Global, Spider, SpiderHost, SpiderTimeline)
import Reflex.UIKit.ViewBuilder (UIKitViewBuilderT)
import Reflex.UIKit.ViewBuilder as ViewBuilder
import UIKit.Types (UIView)


-- t ~ (SpiderTimeline Global)
-- m ~ UIKitViewBuilderT (SpiderTimeline Global) (PerformEventT Spider (SpiderHost Global))

{-# SPECIALIZE mapIntMapWithAdjustImpl :: forall v v'.
     (   (IntMap.Key -> (Event (SpiderTimeline Global) (), v) -> (UIKitViewBuilderT Spider (PerformEventT Spider (SpiderHost Global))) v')
      -> IntMap (Event (SpiderTimeline Global) (), v)
      -> Event (SpiderTimeline Global) (PatchIntMap (Event (SpiderTimeline Global) (), v))
      -> (UIKitViewBuilderT Spider (PerformEventT Spider (SpiderHost Global))) (IntMap v', Event (SpiderTimeline Global) (PatchIntMap v'))
     )
  -> (IntMap.Key -> v -> PostBuildT (SpiderTimeline Global) (UIKitViewBuilderT Spider (PerformEventT Spider (SpiderHost Global))) v')
  -> IntMap v
  -> Event (SpiderTimeline Global) (PatchIntMap v)
  -> PostBuildT (SpiderTimeline Global) (UIKitViewBuilderT Spider (PerformEventT Spider (SpiderHost Global))) (IntMap v', Event (SpiderTimeline Global) (PatchIntMap v'))
  #-}

{-# SPECIALIZE append ::
     UIView
  -> UIKitViewBuilderT (SpiderTimeline Global) (PerformEventT Spider (SpiderHost Global)) ()
  #-}

{-# SPECIALIZE appendFragment ::
     UIView
  -> UIKitViewBuilderT (SpiderTimeline Global) (PerformEventT Spider (SpiderHost Global)) ()
  #-}

{-# SPECIALIZE uikitRunWithReplace :: forall a b.
     UIKitViewBuilderT (SpiderTimeline Global) (PerformEventT Spider (SpiderHost Global)) a
  -> Event (SpiderTimeline Global) (UIKitViewBuilderT (SpiderTimeline Global) (PerformEventT Spider (SpiderHost Global)) b)
  -> UIKitViewBuilderT (SpiderTimeline Global) (PerformEventT Spider (SpiderHost Global)) (a, Event (SpiderTimeline Global) b)
  #-}

{-# SPECIALIZE uikitTraverseIntMapWithKeyWithAdjust :: forall v v'.
     (IntMap.Key -> v -> UIKitViewBuilderT (SpiderTimeline Global) (PerformEventT Spider (SpiderHost Global)) v')
  -> IntMap v
  -> Event (SpiderTimeline Global) (PatchIntMap v)
  -> UIKitViewBuilderT (SpiderTimeline Global) (PerformEventT Spider (SpiderHost Global)) (IntMap v', Event (SpiderTimeline Global) (PatchIntMap v'))
  #-}

{-# SPECIALIZE uikitTraverseDMapWithKeyWithAdjust :: forall k v v'. GCompare k
  => (forall a. k a -> v a -> UIKitViewBuilderT (SpiderTimeline Global) (PerformEventT Spider (SpiderHost Global)) (v' a))
  -> DMap k v
  -> Event (SpiderTimeline Global) (PatchDMap k v)
  -> UIKitViewBuilderT (SpiderTimeline Global) (PerformEventT Spider (SpiderHost Global)) (DMap k v', Event (SpiderTimeline Global) (PatchDMap k v'))
  #-}

{-# SPECIALIZE uikitTraverseDMapWithKeyWithAdjustWithMove :: forall k v v'. GCompare k
  => (forall a. k a -> v a -> UIKitViewBuilderT (SpiderTimeline Global) (PerformEventT Spider (SpiderHost Global)) (v' a))
  -> DMap k v
  -> Event (SpiderTimeline Global) (PatchDMapWithMove k v)
  -> UIKitViewBuilderT (SpiderTimeline Global) (PerformEventT Spider (SpiderHost Global)) (DMap k v', Event (SpiderTimeline Global) (PatchDMapWithMove k v'))
  #-}

