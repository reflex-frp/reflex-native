#import <UIKit/UIKit.h>
#import "HsFFI.h"

UIViewController* uiWindow_getRootViewController(UIWindow* window) {
  return window.rootViewController;
}

void uiWindow_setRootViewController(UIWindow* window, UIViewController* vc) {
  window.rootViewController = vc;
}

BOOL uiWindow_isKeyWindow(UIWindow* window) {
  return window.keyWindow;
}

void uiWindow_makeKeyAndVisible(UIWindow* window) {
  [window makeKeyAndVisible];
}

void uiWindow_makeKeyWindow(UIWindow* window) {
  [window makeKeyWindow];
}

