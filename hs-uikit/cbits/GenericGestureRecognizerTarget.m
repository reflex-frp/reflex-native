#import "GenericGestureRecognizerTarget.h"
#import "UIKit/Generic/GestureRecognizerTarget_stub.h"

@implementation GenericGestureRecognizerTarget

- (id)initWithCallback:(HsStablePtr)callback {
  if (!(self = [super init])) {
    return self;
  }

  _callback = callback;

  return self;
}

- (void)dealloc {
  if (_recognizer == nil) return;
  if (_recognizer.view == nil) return;
  [_recognizer.view removeGestureRecognizer:_recognizer];
}

- (void)handler:(UIGestureRecognizer*)recognizer {
  genericGestureRecognizerTarget_handler(_callback, (__bridge HsPtr)_recognizer);
}

@end

GenericGestureRecognizerTarget* genericGestureRecognizerTarget_new(HsStablePtr callback) {
  return [[GenericGestureRecognizerTarget alloc] initWithCallback:callback];
}

void genericGestureRecognizerTarget_setRecognizer(GenericGestureRecognizerTarget* target, UIGestureRecognizer* recognizer) {
  target.recognizer = recognizer;
}
