#import <UIKit/UIKit.h>
#import "HsFFI.h"

UIPanGestureRecognizer* uiPanGestureRecognizer_new(id target) {
  return [[UIPanGestureRecognizer alloc] initWithTarget:target action:@selector(handler:)];
}

void uiPanGestureRecognizer_getTranslationInSuperview(UIPanGestureRecognizer* recognizer, CGPoint* out) {
  if (recognizer.view == nil || recognizer.view.superview == nil) {
    *out = CGPointZero;
  } else {
    *out = [recognizer translationInView:recognizer.view.superview];
  }
}

void uiPanGestureRecognizer_getVelocityInSuperview(UIPanGestureRecognizer* recognizer, CGPoint* out) {
  if (recognizer.view == nil || recognizer.view.superview == nil) {
    *out = CGPointZero;
  } else {
    *out = [recognizer velocityInView:recognizer.view.superview];
  }
}
