{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- |Reexports of the various @Reflex.Native@ modules and the 'MonadNative' constraint alias for writing shorter but less precise signatures.
module Reflex.Native
  (
  -- * Reexports
    module Export
  -- * @MonadNative@
  , MonadNativeConstraints, MonadNative
  ) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Ref (MonadRef(type Ref))
import Reflex as Export
import Reflex.Host.Class (MonadReflexCreateTrigger)
import Reflex.Native.Color as Export
import Reflex.Native.Font as Export
import Reflex.Native.Geometry as Export
import Reflex.Native.Gesture as Export
import Reflex.Native.TextConfig as Export
import Reflex.Native.TextStyle as Export
import Reflex.Native.ViewBuilder.Class as Export
import Reflex.Native.ViewConfig as Export
import Reflex.Native.ViewLayout as Export
import Reflex.Native.ViewStyle as Export


-- |Grab-bag of constraints which make a fully-featured Reflex Native monad stack.
--
-- Specifically:
--
--     * 'ViewBuilder', granting the ability to build reactive view hierarchies that are cross platform.
--     * 'Adjustable', granting the ability to react to @Event@s by changing the built view hierarchy at various levels of
--     granularity.
--     * 'Reflex', granting essential FRP functionality like @Behavior@, @Event@, etc. and associated functions.
--     * 'MonadFix', granting the ability to make recursive bindings which is especially important for UIs as components often rely on previous state to
--     determine future state and the order of views controls which views overlap others, but the flow of data might not be in the same order.
--     * 'MonadHold', granting additional functionality to hold @Event@s and create @Behavior@s and @Dynamic@s which update when those events fire, allowing
--     stateful behavior.
--     * 'MonadSample', granting the ability to sample the values of @Behavior@s at build time.
--     * 'TriggerEvent' and 'MonadReflexCreateTrigger', granting the ability to create @Event@s which trigger based on externally invoked @IO@ actions.
--     * 'PostBuild', exposing an @Event@ which fires every time a build phase completes.
--     * 'MonadIO', granting the ability to run arbitrary IO actions at build time. Of course, IO actions cannot be rewound so be aware when using @MonadIO@
--     with @Adjustable@.
--     * 'MonadRef', which is redundant with @MonadIO@ available but convenient for generic functions.
--     * 'PerformEvent', granting the ability to execute arbitrary monadic actions in response to @Event@s firing, along with the guarantee that whatever monad
--     is used is @MonadIO@ and @MonadRef@.
--     * @Monad@, @Applicative@, and @Functor@, implied by many of the previous.
type MonadNativeConstraints t m =
  ( ViewBuilder t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  , MonadSample t (Performable m)
  , MonadReflexCreateTrigger t m
  , PostBuild t m
  , MonadIO m
  , TriggerEvent t m
  , MonadRef m, Ref m ~ Ref IO
  , PerformEvent t m
  , MonadIO (Performable m)
  , MonadRef (Performable m), Ref (Performable m) ~ Ref IO
  )

-- |Class which implies all constraints of 'MonadNativeConstraints' but doesn't require additional language extensions at the use site.
class MonadNativeConstraints t m => MonadNative t m | m -> t
-- |Trivial instance for all types @t@, @m@
instance MonadNativeConstraints t m => MonadNative t m

