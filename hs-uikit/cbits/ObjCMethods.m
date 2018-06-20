#import <Foundation/Foundation.h>


void hsobjc_retain(id obj) {
  (void)CFBridgingRetain(obj);
}

void hsobjc_release(const void* obj) {
  (void)CFBridgingRelease(obj);
}

