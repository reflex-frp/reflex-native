#import "GenericAppDelegate.h"
#import <stdint.h>
#import "UIKit/Generic/AppDelegate_stub.h"


@interface GenericAppDelegate ()

@end

static HsStablePtr genericAppDelegate_globalConfig;

@implementation GenericAppDelegate

- (BOOL)application:(UIApplication*)application willFinishLaunchingWithOptions:(NSDictionary*)launchOptions {
  return genericAppDelegate_willFinishLaunchingWithOptions(genericAppDelegate_globalConfig, (__bridge HsPtr)application, (__bridge HsPtr)launchOptions);
}

- (BOOL)application:(UIApplication*)application didFinishLaunchingWithOptions:(NSDictionary*)launchOptions {
  self.window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
  self.window.backgroundColor = [UIColor redColor];
  return genericAppDelegate_didFinishLaunchingWithOptions(genericAppDelegate_globalConfig, (__bridge HsPtr)application, (__bridge HsPtr)launchOptions, (__bridge HsPtr)self.window);
}

- (void)applicationWillResignActive:(UIApplication*)application {
  genericAppDelegate_willResignActive(genericAppDelegate_globalConfig, (__bridge HsPtr)application);
}

- (void)applicationDidEnterBackground:(UIApplication*)application {
  genericAppDelegate_didEnterBackground(genericAppDelegate_globalConfig, (__bridge HsPtr)application);
}

- (void)applicationWillEnterForeground:(UIApplication*)application {
  genericAppDelegate_willEnterForeground(genericAppDelegate_globalConfig, (__bridge HsPtr)application);
}

- (void)applicationDidBecomeActive:(UIApplication*)application {
  genericAppDelegate_didBecomeActive(genericAppDelegate_globalConfig, (__bridge HsPtr)application);
}

- (void)applicationWillTerminate:(UIApplication*)application {
  genericAppDelegate_willTerminate(genericAppDelegate_globalConfig, (__bridge HsPtr)application);
}

- (void)applicationSignificantTimeChange:(UIApplication*)application {
  genericAppDelegate_significantTimeChange(genericAppDelegate_globalConfig, (__bridge HsPtr)application);
}

/*
- (void)application:(UIApplication*)application didRegisterForRemoteNotificationsWithDeviceToken:(NSData*)deviceToken {
}

- (void)application:(UIApplication*)application didReceiveRemoteNotification:(NSDictionary*)userInfo fetchCompletionHandler:(void (^)(UIBackgroundFetchResult result))completionHandler {
}

- (void)application:(UIApplication*)application didFailToRegisterForRemoteNotificationsWithError:(NSError*)error {
}

- (BOOL)application:(UIApplication*)application continueUserActivity:(NSUserActivity*)userActivity restorationHandler:(void (^)(NSArray *restorableObjects))restorationHandler {
}
*/

@end

void genericAppDelegate_runApplication(HsStablePtr config) {
  @autoreleasepool {
    genericAppDelegate_globalConfig = config;
    char* _Nonnull argv [] =  {"", 0};
    UIApplicationMain(0, argv, nil, NSStringFromClass([GenericAppDelegate class]));
  }
}

