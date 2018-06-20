{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Reflex.Native.Examples.Draggy where

import Data.AdditiveGroup ((^+^), zeroV)
import Data.Functor (($>))
import Data.Functor.Identity (Identity(..))
import Data.Maybe (Maybe(Just, Nothing))
import Reflex (attachWith, current, ffor, fforMaybeCheap, fmapMaybeCheap, getPostBuild, holdDyn, updated, zipDynWith)
import Reflex.Native
  ( MonadNative, ViewBuilder(type ViewBuilderSpace, buildTextView, buildView, recognizeGesture, wrapRawView), ViewSpace(type RawView)
  , GestureSpec(..), GestureState(..), PanGesture(..), _gestureState_data
  , Point(..), Rect(..), Size(..), lightGray, darkGray
  , TextConfig(..), defaultTextConfig
  , RawViewConfig(..), defaultRawViewConfig
  , ViewConfig(..), defaultViewConfig
  , ViewLayout(..)
  , ViewStyle(..), defaultModifyViewStyle
  )


main :: forall t m. MonadNative t m => RawView (ViewBuilderSpace m) -> m ()
main rootRawView = do
  pb <- getPostBuild
  let rootViewConfig = defaultRawViewConfig
        { _rawViewConfig_modifyStyle = Just $ (defaultModifyViewStyle @t)
            { _viewStyle_backgroundColor = pb $> lightGray }
        }
  _rootView <- wrapRawView rootRawView rootViewConfig

  rec
    let pos0 = Point 10 10
    (vn, _) <- buildView
      (defaultViewConfig
        { _viewConfig_initialStyle = ViewStyle (Identity darkGray)
        , _viewConfig_initialLayout = ViewLayout_Fixed (Rect pos0 (Size 100 100))
        , _viewConfig_modifyLayout = Just $ ffor (updated pos) $ \ p -> ViewLayout_Fixed (Rect p (Size 100 100))
        , _viewConfig_initialAccessibilityLabel = Just "test view"
        }) $ do
        _ <- buildTextView $ defaultTextConfig
          { _textConfig_viewConfig = defaultViewConfig { _viewConfig_initialLayout = ViewLayout_Fixed (Rect (Point 0 0) (Size 100 100)) }
          , _textConfig_initialText = "drag me!"
          }
        pure ()
    panState <- recognizeGesture vn GestureSpec_Pan
    lastStartPos <- holdDyn pos0 $ attachWith const (current pos) $ fforMaybeCheap panState $ \ case
      GestureState_Began _ -> Just ()
      _                    -> Nothing
    lastDragTranslation <- holdDyn zeroV $ fmapMaybeCheap (fmap _panGesture_translation . _gestureState_data) panState
    let pos = zipDynWith (^+^) lastStartPos lastDragTranslation

  pure ()



