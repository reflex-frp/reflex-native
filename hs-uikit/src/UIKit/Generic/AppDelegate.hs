{-# LANGUAGE RecordWildCards #-}
-- |Generic @UIApplication@ delegate implementation using explicit configuration and callbacks.
module UIKit.Generic.AppDelegate
  (
  -- * Configuring
    AppDelegateConfig(..), defaultAppDelegateConfig
  -- * Running a @UIApplication@
  , runGenericApplication
  -- * Raw FFI bindings
  , genericAppDelegate_runApplication
  ) where

import Control.Applicative ((<$>), pure)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (Bool(True))
import Foreign.C.Types (CChar(..))
import Foreign.Ptr (Ptr)
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr)
import Foundation (NSDictionary, NSDictionaryType)
import ObjC (retainObj, objcBool)
import UIKit.Types (MainThread(..), UIApplication, UIApplicationType, UIWindow, UIWindowType)


-- |Configuration for an app delegate which gives some user-specified additional data @a@ which will be held as long as the app delegate is live along with
-- callbacks to invoke during the various lifecycle events of the application.
--
-- __Warning:__ take care to not refer back to the @UIApplication@ strongly from @a@, as that will cause a reference cycle which will never be deallocated. Of
-- course, applications typically run the entire lifetime of the process so it's not that bad in practice.
data AppDelegateConfig a = AppDelegateConfig
  { _appDelegate_a :: a
  -- ^The user-specified data to hold while the app delegate is live.
  --
  -- __Warning:__ take care to not refer back to the @UIApplication@ strongly from @a@, as that will cause a reference cycle which will never be deallocated. Of
  -- course, applications typically run the entire lifetime of the process so it's not that bad in practice.
  , _appDelegate_willFinishLaunchingWithOptions :: a -> UIApplication -> NSDictionary -> MainThread Bool
  -- ^Callback to invoke when @UIApplicationDelegate - application:willFinishLaunchingWithOptions:@ is called. Invoked with the user-specified data @a@
  -- ('_appDelegate_a'), the @UIApplication@ that will launch, and the launch options as a @NSDictionary@. Can return @False@ if the launch options indicate
  -- the application is being launched to handle a URI and the application shouldn't handle the URI.
  , _appDelegate_didFinishLaunchingWithOptions :: a -> UIApplication -> NSDictionary -> UIWindow -> MainThread Bool
  -- ^Callback to invoke when @UIApplicationDelegate - application:didFinishLaunchingWithOptions:@ is called. Invoked with the user-specified data @a@
  -- ('_appDelegate_a'), the @UIApplication@ that did launch, and the launch options as a @NSDictionary@. Can return @False@ if the launch options indicate
  -- the application is being launched to handle a URI and the application shouldn't handle the URI.
  , _appDelegate_didBecomeActive :: a -> UIApplication -> MainThread ()
  -- ^Callback to invoke when @UIApplicationDelegate - applicationDidBecomeActive:@ is called. Invoked with the user-specified data @a@
  -- ('_appDelegate_a'), and the @UIApplication@ that did become active.
  , _appDelegate_willResignActive :: a -> UIApplication -> MainThread ()
  -- ^Callback to invoke when @UIApplicationDelegate - applicationWillResignActive:@ is called. Invoked with the user-specified data @a@
  -- ('_appDelegate_a'), and the @UIApplication@ that will resign being active.
  , _appDelegate_didEnterBackground :: a -> UIApplication -> MainThread ()
  -- ^Callback to invoke when @UIApplicationDelegate - applicationDidEnterBackground:@ is called. Invoked with the user-specified data @a@
  -- ('_appDelegate_a'), and the @UIApplication@ that will enter the background.
  , _appDelegate_willEnterForeground :: a -> UIApplication -> MainThread ()
  -- ^Callback to invoke when @UIApplicationDelegate - applicationWillEnterForeground:@ is called. Invoked with the user-specified data @a@
  -- ('_appDelegate_a'), and the @UIApplication@ that will enter the foreground.
  , _appDelegate_willTerminate :: a -> UIApplication -> MainThread ()
  -- ^Callback to invoke when @UIApplicationDelegate - applicationWillTerminate:@ is called. Invoked with the user-specified data @a@
  -- ('_appDelegate_a'), and the @UIApplication@ that will terminate.
  , _appDelegate_significantTimeChange :: a -> UIApplication -> MainThread ()
  -- ^Callback to invoke when @UIApplicationDelegate - applicationSignificantTimeChange:@ is called. Invoked with the user-specified data @a@
  -- ('_appDelegate_a'), and the @UIApplication@ that experienced a significant wallclock time change.
  }

-- |Create an 'AppDelegateConfig' with the given @a@ and no-op callbacks.
defaultAppDelegateConfig :: a -> AppDelegateConfig a
defaultAppDelegateConfig a = AppDelegateConfig
  { _appDelegate_a                              = a
  , _appDelegate_willFinishLaunchingWithOptions = \ _ _ _ -> pure True
  , _appDelegate_didFinishLaunchingWithOptions  = \ _ _ _ _ -> pure True
  , _appDelegate_didBecomeActive                = \ _ _ -> pure ()
  , _appDelegate_willResignActive               = \ _ _ -> pure ()
  , _appDelegate_didEnterBackground             = \ _ _ -> pure ()
  , _appDelegate_willEnterForeground            = \ _ _ -> pure ()
  , _appDelegate_willTerminate                  = \ _ _ -> pure ()
  , _appDelegate_significantTimeChange          = \ _ _ -> pure ()
  }

-- |Raw FFI callback to @genericAppDelegate_runApplication@ which invokes @UIApplicationMain@ with a new app delegate configured with the given configuration.
foreign import ccall genericAppDelegate_runApplication :: StablePtr (AppDelegateConfig a) -> IO ()

-- |Launch an application using @UIApplicationMain@ and the @GenericAppDelegate@ class which will use the given 'AppDelegateConfig'. This is typically used
-- to implement @main@.
--
-- __Note:__ The underlying ObjC code uses a global variable to pass the configuration into the newly created @GenericAppDelegate@ as it is not possible to
-- create the app delegate directly. Because of this, it's possible that concurrent calls to @runGenericApplication@ (which should never occur anyways!) would
-- clobber each other's configuration. Since there should be only one @UIApplicationMain@ call ever in a process, that situation should never be a problem.
runGenericApplication :: AppDelegateConfig a -> IO b
runGenericApplication cfg = do
  ptr <- newStablePtr cfg -- leaks, which is probably fine
  genericAppDelegate_runApplication ptr
  error "UIApplicationMain should never return"

foreign export ccall genericAppDelegate_willFinishLaunchingWithOptions
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> Ptr NSDictionaryType -> MainThread CChar
-- |Callback from ObjC which triggers the '_appDelegateConfig_willFinishLaunchingWithOptions' callback.
genericAppDelegate_willFinishLaunchingWithOptions
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> Ptr NSDictionaryType -> MainThread CChar
genericAppDelegate_willFinishLaunchingWithOptions configPtr applicationPtr launchOptionsPtr = do
  application <- retainObj applicationPtr
  launchOptions <- retainObj launchOptionsPtr
  AppDelegateConfig {..} <- liftIO $ deRefStablePtr configPtr
  objcBool <$> _appDelegate_willFinishLaunchingWithOptions _appDelegate_a application launchOptions

foreign export ccall genericAppDelegate_didFinishLaunchingWithOptions
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> Ptr NSDictionaryType -> Ptr UIWindowType -> MainThread CChar
-- |Callback from ObjC which triggers the '_appDelegateConfig_didFinishLaunchingWithOptions' callback.
genericAppDelegate_didFinishLaunchingWithOptions
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> Ptr NSDictionaryType -> Ptr UIWindowType -> MainThread CChar
genericAppDelegate_didFinishLaunchingWithOptions configPtr applicationPtr launchOptionsPtr windowPtr = do
  application <- retainObj applicationPtr
  launchOptions <- retainObj launchOptionsPtr
  window <- retainObj windowPtr
  AppDelegateConfig {..} <- liftIO $ deRefStablePtr configPtr
  objcBool <$> _appDelegate_didFinishLaunchingWithOptions _appDelegate_a application launchOptions window

foreign export ccall genericAppDelegate_didBecomeActive
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> MainThread ()
-- |Callback from ObjC which triggers the '_appDelegateConfig_didBecomeActive' callback.
genericAppDelegate_didBecomeActive
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> MainThread ()
genericAppDelegate_didBecomeActive configPtr applicationPtr = do
  application <- retainObj applicationPtr
  AppDelegateConfig {..} <- liftIO $ deRefStablePtr configPtr
  _appDelegate_didBecomeActive _appDelegate_a application

foreign export ccall genericAppDelegate_willResignActive
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> MainThread ()
-- |Callback from ObjC which triggers the '_appDelegateConfig_willResignActive' callback.
genericAppDelegate_willResignActive
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> MainThread ()
genericAppDelegate_willResignActive configPtr applicationPtr = do
  application <- retainObj applicationPtr
  AppDelegateConfig {..} <- liftIO $ deRefStablePtr configPtr
  _appDelegate_willResignActive _appDelegate_a application

foreign export ccall genericAppDelegate_didEnterBackground
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> MainThread ()
-- |Callback from ObjC which triggers the '_appDelegateConfig_didEnterBackground' callback.
genericAppDelegate_didEnterBackground
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> MainThread ()
genericAppDelegate_didEnterBackground configPtr applicationPtr = do
  application <- retainObj applicationPtr
  AppDelegateConfig {..} <- liftIO $ deRefStablePtr configPtr
  _appDelegate_didEnterBackground _appDelegate_a application

foreign export ccall genericAppDelegate_willEnterForeground
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> MainThread ()
-- |Callback from ObjC which triggers the '_appDelegateConfig_willEnterForeground' callback.
genericAppDelegate_willEnterForeground
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> MainThread ()
genericAppDelegate_willEnterForeground configPtr applicationPtr = do
  application <- retainObj applicationPtr
  AppDelegateConfig {..} <- liftIO $ deRefStablePtr configPtr
  _appDelegate_willEnterForeground _appDelegate_a application

foreign export ccall genericAppDelegate_willTerminate
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> MainThread ()
-- |Callback from ObjC which triggers the '_appDelegateConfig_willTerminate' callback.
genericAppDelegate_willTerminate
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> MainThread ()
genericAppDelegate_willTerminate configPtr applicationPtr = do
  application <- retainObj applicationPtr
  AppDelegateConfig {..} <- liftIO $ deRefStablePtr configPtr
  _appDelegate_willTerminate _appDelegate_a application

foreign export ccall genericAppDelegate_significantTimeChange
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> MainThread ()
-- |Callback from ObjC which triggers the '_appDelegateConfig_significantTimeChange' callback.
genericAppDelegate_significantTimeChange
  :: StablePtr (AppDelegateConfig a) -> Ptr UIApplicationType -> MainThread ()
genericAppDelegate_significantTimeChange configPtr applicationPtr = do
  application <- retainObj applicationPtr
  AppDelegateConfig {..} <- liftIO $ deRefStablePtr configPtr
  _appDelegate_significantTimeChange _appDelegate_a application
