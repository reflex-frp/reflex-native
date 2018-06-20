{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- |Pointer types and type tokens from the @UIKit@ framework, along with an abstraction for making @UIKit@ functions and methods which must only run on the
-- main (UI) thread more safe.
module UIKit.Types
  (
  -- * 'MainThread' and associated functionality
    MainThread(..), unsafeInMainThread, checkMainThread, isMainThread, dispatchAsyncMain
  -- * Pointer types
  , UIApplication, UIColor, UIFont, UIGestureRecognizer, UIPanGestureRecognizer, UILabel, UIView, UIViewController, UIWindow
  -- * Type coercions
  , asUIGestureRecognizer, asUIView, asUIViewController
  -- * Type tokens
  , UIApplicationType, UIColorType, UIFontType, UIGestureRecognizerType, UIPanGestureRecognizerType, UILabelType, UIViewType, UIViewControllerType, UIWindowType
  -- * Raw FFI bindings
  , mainThread_checkMainThread, mainThread_isMainThread, mainThread_inMainThread, mainThread_dispatchAsyncMain
  ) where

import Control.Applicative (Applicative, Alternative)
import Control.Monad (Monad, MonadPlus, (<=<))
import Control.Monad.Base (MonadBase)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Functor (Functor)
import Foreign.C.Types (CChar(..))
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import ObjC (ObjPtr, ObjType, SafeObjCoerce, coerceObj, unObjcBool)


-- |Wrapped 'IO' action which should only ever be executed on the main UIKit thread.
--
-- See 'dispatchAsyncMain' for a way to schedule a @MainThread@ action on the main thread using Grand Central Dispatch.
-- See 'isMainThread' for a way to test if the currently executing thread is the main thread.
-- See 'unsafeInMainThread' for a way to execute a @MainThread@ action on the current thread, hopefully after checking that it's the main thread.
newtype MainThread a = MainThread { unsafeUnMainThread :: IO a }
deriving instance Alternative MainThread
deriving instance Applicative MainThread
deriving instance Functor MainThread
deriving instance Monad MainThread
deriving instance MonadBase IO MainThread
deriving instance MonadBaseControl IO MainThread
deriving instance MonadFail MainThread
deriving instance MonadFix MainThread
deriving instance MonadIO MainThread
deriving instance MonadPlus MainThread

-- |Unwrap a 'MainThread' via 'unsafeUnMainThread' and 'liftIO' it. Unsafe because there's no check that the thread the resulting @m a@ executes on is in fact
-- the main thread.
unsafeInMainThread :: MonadIO m => MainThread a -> m a
unsafeInMainThread = liftIO . unsafeUnMainThread

-- |Raw FFI binding which checks if the current thread is the main thread and logs a warning (via @os_log@) if not.
foreign import ccall unsafe mainThread_checkMainThread :: IO ()
-- |Check if the currently executing thread is the main thread and log a warning via @os_log@ if not. Used at key entry points to provide a point to diagnose
-- issues with actions running on the wrong thread.
checkMainThread :: IO ()
checkMainThread = mainThread_checkMainThread

-- |Raw FFI binding to @NSThread + isMainThread@.
foreign import ccall unsafe mainThread_isMainThread :: IO CChar
-- |@NSThread + isMainThread@ - return @True@ iff the currently executing thread is the main UIKit thread, and thus 'MainThread' actions are safe to execute.
isMainThread :: IO Bool
isMainThread = unObjcBool <$> mainThread_isMainThread

foreign export ccall mainThread_inMainThread :: StablePtr (MainThread ()) -> IO ()
-- |Raw FFI callback from C code to execute a 'MainThread' action referred to by a 'StablePtr'.
mainThread_inMainThread :: StablePtr (MainThread ()) -> IO ()
mainThread_inMainThread callbackPtr = do
  callback <- liftIO $ deRefStablePtr callbackPtr
  liftIO $ freeStablePtr callbackPtr
  unsafeUnMainThread callback

-- |Raw FFI binding to @dispatch_async(dispatch_get_main_queue(), ^{ … })@.
foreign import ccall unsafe mainThread_dispatchAsyncMain :: StablePtr (MainThread ()) -> IO ()
-- |Arrange for a 'MainThread' action to be later executed on the main UIKit thread using the Grand Central Dispatch @dispatch_async@ function.
--
-- The action will be executed approximately when the next @UIApplicationMain@ top level loop iteration occurs, though GCD does not give precise guarantees on
-- whether that will be the next iteration or some later one.
dispatchAsyncMain :: MainThread () -> IO ()
dispatchAsyncMain = mainThread_dispatchAsyncMain <=< newStablePtr

-- |Safely coerce an object pointer to 'UIGestureRecognizer'.
asUIGestureRecognizer :: SafeObjCoerce a UIGestureRecognizerType => ObjPtr a -> UIGestureRecognizer
asUIGestureRecognizer = coerceObj

-- |Safely coerce an object pointer to 'UIView'.
asUIView :: SafeObjCoerce a UIViewType => ObjPtr a -> UIView
asUIView = coerceObj

-- |Safely coerce an object pointer to 'UIViewController'.
asUIViewController :: SafeObjCoerce a UIViewControllerType => ObjPtr a -> UIViewController
asUIViewController = coerceObj

-- |Type token representing @UIApplication@.
data UIApplicationType
-- |Pointer to a @UIApplication@ instance. See "UIKit.UIApplication" for methods pertaining to @UIColor@.
type UIApplication = ObjPtr UIApplicationType
instance SafeObjCoerce UIApplicationType ObjType

-- |Type token representing @UIColor@.
data UIColorType
-- |Pointer to a @UIColor@ instance. See "UIKit.UIColor" for methods pertaining to @UIColor@.
type UIColor = ObjPtr UIColorType
instance SafeObjCoerce UIColorType ObjType

-- |Type token representing @UIFont@.
data UIFontType
-- |Pointer to a @UIFont@ instance. See "UIKit.UIFont" for methods pertaining to @UIFont@.
type UIFont = ObjPtr UIFontType
instance SafeObjCoerce UIFontType ObjType

-- |Type token representing @UIGestureRecognizer@.
data UIGestureRecognizerType
-- |Pointer to a @UIGestureRecognizer@ instance. See "UIKit.UIGestureRecognizer" for methods pertaining to @UIGestureRecognizer@ and its subclasses.
type UIGestureRecognizer = ObjPtr UIGestureRecognizerType
instance SafeObjCoerce UIGestureRecognizerType ObjType

-- |Type token representing @UIGestureRecognizer@.
data UIPanGestureRecognizerType
-- |Pointer to a @UIGestureRecognizer@ instance. See "UIKit.UIGestureRecognizer" for methods pertaining to @UIGestureRecognizer@ and its subclasses.
type UIPanGestureRecognizer = ObjPtr UIPanGestureRecognizerType
instance SafeObjCoerce UIPanGestureRecognizerType ObjType
instance SafeObjCoerce UIPanGestureRecognizerType UIGestureRecognizerType

-- |Type token representing @UILabel@.
data UILabelType
-- |Pointer to a @UILabel@ instance. See "UIKit.UILabel" for methods pertaining to @UILabel@ and its subclasses.
type UILabel = ObjPtr UILabelType
instance SafeObjCoerce UILabelType ObjType
instance SafeObjCoerce UILabelType UIViewType

-- |Type token representing @UIView@.
data UIViewType
-- |Pointer to a @UIView@ instance. See "UIKit.UIView" for methods pertaining to @UIView@ and its subclasses.
type UIView = ObjPtr UIViewType
instance SafeObjCoerce UIViewType ObjType

-- |Type token representing @UIViewController@.
data UIViewControllerType
-- |Pointer to a @UIViewController@ instance. See "UIKit.UIViewController" for methods pertaining to @UIViewController@ and its subclasses.
type UIViewController = ObjPtr UIViewControllerType
instance SafeObjCoerce UIViewControllerType ObjType

-- |Type token representing @UIWindow@.
data UIWindowType
-- |Pointer to a @UIWindow@ instance. See "UIKit.UIWindow" for methods pertaining to @UIWindow@ and its subclasses.
type UIWindow = ObjPtr UIWindowType
instance SafeObjCoerce UIWindowType ObjType
instance SafeObjCoerce UIWindowType UIViewType
