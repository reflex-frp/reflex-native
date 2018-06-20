#import <os/log.h>
#import <UIKit/UIKit.h>
#import "UIKit/Types_stub.h"

void mainThread_checkMainThread() {
  if (![NSThread isMainThread]) {
    os_log_error(OS_LOG_DEFAULT, "expected to be on main thread for builder actions :'(");
  }
}

BOOL mainThread_isMainThread() {
  return [NSThread isMainThread];
}

void mainThread_dispatchAsyncMain(HsStablePtr callbackPtr) {
  dispatch_async(dispatch_get_main_queue(), ^{
    mainThread_inMainThread(callbackPtr);
  });
}
