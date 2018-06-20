#import <UIKit/UIKit.h>
#import "HsFFI.h"


static int uiGestureRecognizer_stateToHs(UIGestureRecognizerState state) {
  switch (state) {
    case UIGestureRecognizerStatePossible:   return 0;
    case UIGestureRecognizerStateBegan:      return 1;
    case UIGestureRecognizerStateChanged:    return 2;
    case UIGestureRecognizerStateEnded:      return 3;
    case UIGestureRecognizerStateCancelled:  return 4;
    default: return 5; /* Failed */
  }
}

int uiGestureRecognizer_getState(UIGestureRecognizer* recognizer) {
  return uiGestureRecognizer_stateToHs(recognizer.state);
}


