{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
-- |Functions for applying 'ViewConfig' settings to UIKit views.
module Reflex.UIKit.Config where

import Control.Monad ((=<<), (<=<))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Traversable (traverse)
import qualified Foundation.NSString as NSString
import ObjC (SafeObjCoerce, ObjPtr, ObjType)
import Reflex (Reflex, Requester(type Request), requesting_)
import Reflex.Native (ViewConfig(..))
import Reflex.UIKit.Layout (applyLayout)
import Reflex.UIKit.Style (applyViewStyle, initialStyle, modifyStyle)
import UIKit.Types (MainThread, UIViewType)
import qualified UIKit.UIAccessibility as UIAccessibility


-- |Apply the initial settings of a 'ViewConfig' to the given view.
{-# INLINABLE applyInitialViewConfig #-}
applyInitialViewConfig :: (SafeObjCoerce v ObjType, SafeObjCoerce v UIViewType) => ObjPtr v -> ViewConfig t -> MainThread ()
applyInitialViewConfig v (ViewConfig {..}) = do
  applyViewStyle initialStyle v _viewConfig_initialStyle
  UIAccessibility.setAccessibilityLabel v =<< traverse (liftIO . NSString.fromText) _viewConfig_initialAccessibilityLabel
  applyLayout v _viewConfig_initialLayout

-- |Apply updates from the @set@ settings of a 'ViewConfig' as the @Event@s fire, using a 'Requester'.
{-# INLINABLE applyModifyViewConfig #-}
applyModifyViewConfig
  :: (SafeObjCoerce v ObjType, SafeObjCoerce v UIViewType, Reflex t, Monad m, Requester t m, Request m ~ MainThread)
  => ObjPtr v -> ViewConfig t -> m ()
applyModifyViewConfig v (ViewConfig {..}) = do
  for_ _viewConfig_modifyStyle $
    applyViewStyle modifyStyle v
  for_ _viewConfig_modifyAccessibilityLabel $
    requesting_ . fmap (UIAccessibility.setAccessibilityLabel v <=< traverse (liftIO . NSString.fromText))
  for_ _viewConfig_modifyLayout $
    requesting_ . fmap (applyLayout v)
