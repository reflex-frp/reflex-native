{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |Cross platform building of view hierarchies with reactive style, layout, and hierarchy.
module Reflex.Native.ViewBuilder.Class where

import Control.Monad (Monad)
import Control.Monad.Reader (ReaderT(..), ask)
import qualified Control.Monad.RWS.Strict as RWSStrict
import qualified Control.Monad.RWS.Lazy as RWSLazy
import qualified Control.Monad.State.Strict as StateStrict
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Control.Monad.Writer.Lazy as WriterLazy
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Monoid (Monoid)
import Reflex.Class (Event, Reflex)
import Reflex.DynamicWriter (DynamicWriterT(..))
import Reflex.EventWriter (EventWriterT(..))
import Reflex.Native.Gesture (GestureData, GestureSpec, GestureState)
import Reflex.Native.TextConfig (TextConfig)
import Reflex.Native.ViewConfig (ViewConfig, RawViewConfig)
import Reflex.PostBuild.Base (PostBuildT(..))
import Reflex.Query.Base (QueryT(..))
import Reflex.Requester.Base (RequesterT(..))


-- |Class of types which denote a particular "view space" or particular underlying view system in use, e.g. @UIKitViewSpace@, @TestViewSpace@, or
-- @AndroidViewSpace@, which associate types for each of the standard view types in that underlying view system.
class ViewSpace space where
  -- |The type of text views in the underlying view system, e.g. @UILabel@ on UIKit or @TextView@ on Android.
  type RawTextView space :: *

  -- |The type of any arbitrary view in the underlying view system that can be installed via 'placeRawView' or installed and made Reflex-aware using
  -- 'wrapRawView', e.g. @UIView@ on UIKit or @View@ on Android.
  type RawView space :: *

-- |Wrapper around a 'RawTextView' for a given 'ViewSpace'.
newtype TextView space t = TextView { _buildTextView_raw :: RawTextView space }

-- |Wrapper around a 'RawView' for a given 'ViewSpace'.
newtype View space t = View { _buildView_raw :: RawView space }

-- |Typeclass for monads used to build view hierarchies which react over time to events in a cross-platform way. A function being polymorphic over
-- @ViewBuilder t m@ means it should work identically on any supported platform.
class (Monad m, Reflex t, ViewSpace (ViewBuilderSpace m)) => ViewBuilder t m | m -> t where
  -- |The associated 'ViewSpace' for this builder monad.
  type ViewBuilderSpace m :: *

  -- |Create a static text view with the given configuration and place it in the hierarchy.
  buildTextView :: TextConfig t -> m (TextView (ViewBuilderSpace m) t)

  -- |Create a view containing some child hierarchy, returning the created view along with whatever the result of the inner build was.
  buildView :: ViewConfig t -> m a -> m (View (ViewBuilderSpace m) t, a)

  -- |Place a 'RawView' created externally in the view hierarchy being built, for example with functions or libraries that know the precise type of view
  -- hierarchy in use.
  --
  -- Behavior is undefined if the given view node is already in the view hierarchy somewhere else, though each specific view hierarchy has a defined behavior.
  placeRawView :: RawView (ViewBuilderSpace m) -> m ()

  -- |Wrap a 'RawView' for the appropriate 'ViewSpace' with Reflex functionality configured via the given 'RawViewConfig', such as the ability to change the
  -- view style or layout in response to @Event@s or recognize gestures using 'recognizeGesture'.
  --
  -- Behavior of a view wrapped twice will probably not be what you expect; updates associated with later invocations of @wrapRawView@ will probably stomp
  -- earlier invocations of @wrapRawView@, though it is undefined for any @ViewBuilder t m@ if that is so, and even if so which property updates will be
  -- applied.
  wrapRawView :: RawView (ViewBuilderSpace m) -> RawViewConfig t -> m (View (ViewBuilderSpace m) t)

  -- |Given some gesture to recognize and any parameters of that recognition, return an @Event@ which fires each time the state of recognition of the gesture
  -- on the given view changes.
  --
  -- For example,
  --
  -- @
  --   do
  --     e <- recognizeGesture v GestureSpec_Pan
  --     _ <- buildTextView (defaultTextConfig { _textConfig_setText = show <$> e })
  -- @
  --
  -- Will show the state of any pan gesture occuring on @v@: @GestureState_None@ initially then @GestureState_Began …@ when the user starts dragging their
  -- finger across @v@, @GestureState_Changed …@ regularly while the user continues to slide their finger, and @GestureState_Ended …@ when the user lifts their
  -- finger.
  --
  -- __Warning:__ the returned @Event@ is only guaranteed to be valid in the current builder scope. It may (or may not) fire after the current scope is removed
  -- by way of 'Reflex.Class.Adjustable' methods such as 'Reflex.Class.runWithReplace'.
  recognizeGesture :: View (ViewBuilderSpace m) t -> GestureSpec gs -> m (Event t (GestureState (GestureData gs)))

  {-# INLINABLE buildTextView #-}
  default buildTextView
    :: (MonadTrans f, m ~ f n, ViewBuilderSpace n ~ ViewBuilderSpace m, ViewBuilder t n, Monad n)
    => TextConfig t -> m (TextView (ViewBuilderSpace m) t)
  buildTextView cfg = lift $ buildTextView cfg

  {-# INLINABLE placeRawView #-}
  default placeRawView
    :: (MonadTrans f, m ~ f n, ViewBuilderSpace n ~ ViewBuilderSpace m, ViewBuilder t n, Monad n)
    => RawView (ViewBuilderSpace m) -> m ()
  placeRawView v = lift $ placeRawView v

  {-# INLINABLE wrapRawView #-}
  default wrapRawView
    :: (MonadTrans f, m ~ f n, ViewBuilderSpace n ~ ViewBuilderSpace m, ViewBuilder t n, Monad n)
    => RawView (ViewBuilderSpace m) -> RawViewConfig t -> m (View (ViewBuilderSpace m) t)
  wrapRawView v cfg = lift $ wrapRawView v cfg

  {-# INLINABLE recognizeGesture #-}
  default recognizeGesture
    :: (MonadTrans f, m ~ f n, ViewBuilderSpace n ~ ViewBuilderSpace m, ViewBuilder t n, Monad n)
    => View (ViewBuilderSpace m) t -> GestureSpec gs -> m (Event t (GestureState (GestureData gs)))
  recognizeGesture v spec = lift $ recognizeGesture v spec

-- |Pass through 'PostBuildT'.
instance (ViewBuilder t m, Monad m) => ViewBuilder t (PostBuildT t m) where
  type ViewBuilderSpace (PostBuildT t m) = ViewBuilderSpace m

  buildView cfg (PostBuildT body) = PostBuildT $ buildView cfg body

-- |Pass through 'ReaderT'.
instance (ViewBuilder t m, Monad m) => ViewBuilder t (ReaderT r m) where
  type ViewBuilderSpace (ReaderT r m) = ViewBuilderSpace m
  buildView cfg body = do
    r <- ask
    (vn, a) <- lift $ buildView cfg (runReaderT body r)
    pure (vn, a)

-- |Pass through lazy 'WriterT'.
instance (ViewBuilder t m, Monoid w, Monad m) => ViewBuilder t (WriterLazy.WriterT w m) where
  type ViewBuilderSpace (WriterLazy.WriterT w m) = ViewBuilderSpace m
  buildView cfg body = do
    (vn, (a, w)) <- lift $ buildView cfg (WriterLazy.runWriterT body)
    WriterLazy.tell w
    pure (vn, a)

-- |Pass through strict 'WriterT'.
instance (ViewBuilder t m, Monoid w, Monad m) => ViewBuilder t (WriterStrict.WriterT w m) where
  type ViewBuilderSpace (WriterStrict.WriterT w m) = ViewBuilderSpace m
  buildView cfg body = do
    (vn, (a, w)) <- lift $ buildView cfg (WriterStrict.runWriterT body)
    WriterStrict.tell w
    pure (vn, a)

-- |Pass through lazy 'StateLazy.StateT'.
instance (ViewBuilder t m, Monad m) => ViewBuilder t (StateLazy.StateT s m) where
  type ViewBuilderSpace (StateLazy.StateT s m) = ViewBuilderSpace m
  buildView cfg body = do
    s <- StateLazy.get
    (vn, (a, s')) <- lift $ buildView cfg (StateLazy.runStateT body s)
    StateLazy.put s'
    pure (vn, a)

-- |Pass through strict 'StateStrict.StateT'.
instance (ViewBuilder t m, Monad m) => ViewBuilder t (StateStrict.StateT s m) where
  type ViewBuilderSpace (StateStrict.StateT s m) = ViewBuilderSpace m
  buildView cfg body = do
    s <- StateStrict.get
    (vn, (a, s')) <- lift $ buildView cfg (StateStrict.runStateT body s)
    StateStrict.put s'
    pure (vn, a)

-- |Pass through lazy 'RWSLazy.RWST'.
instance (ViewBuilder t m, Monoid w, Monad m) => ViewBuilder t (RWSLazy.RWST r w s m) where
  type ViewBuilderSpace (RWSLazy.RWST r w s m) = ViewBuilderSpace m
  buildView cfg body = do
    r <- RWSLazy.ask
    s <- RWSLazy.get
    (vn, (a, s', w)) <- lift $ buildView cfg (RWSLazy.runRWST body r s)
    RWSLazy.put s'
    RWSLazy.tell w
    pure (vn, a)

-- |Pass through strict 'RWSStrict.RWST'.
instance (ViewBuilder t m, Monoid w, Monad m) => ViewBuilder t (RWSStrict.RWST r w s m) where
  type ViewBuilderSpace (RWSStrict.RWST r w s m) = ViewBuilderSpace m
  buildView cfg body = do
    r <- RWSStrict.ask
    s <- RWSStrict.get
    (vn, (a, s', w)) <- lift $ buildView cfg (RWSStrict.runRWST body r s)
    RWSStrict.put s'
    RWSStrict.tell w
    pure (vn, a)

deriving instance (ViewBuilder t m, Monad m) => ViewBuilder t (DynamicWriterT t w m)
deriving instance (ViewBuilder t m, Monad m) => ViewBuilder t (EventWriterT t w m)
deriving instance (ViewBuilder t m, Monad m) => ViewBuilder t (QueryT t w m)
deriving instance (ViewBuilder t m, Monad m) => ViewBuilder t (RequesterT t req res m)

