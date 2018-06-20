#import <UIKit/UIKit.h>
#import "HsFFI.h"


void uiView_addGestureRecognizer(UIView* view, UIGestureRecognizer* recognizer) {
  [view addGestureRecognizer:recognizer];
}

void uiView_addSubview(UIView* view, UIView* subview) {
  [view addSubview:subview];
}

void uiView_removeFromSuperview(UIView* view) {
  [view removeFromSuperview];
}

void uiView_setAutoresizesSubviews(UIView* view, BOOL does) {
  view.autoresizesSubviews = does;
}

void uiView_setAutoresizingMask(UIView* view, UIViewAutoresizing mask) {
  view.autoresizingMask = mask;
}

void uiView_setBackgroundColor(UIView* view, UIColor* color) {
  view.backgroundColor = color;
  CGFloat r, g, b, a;
  (void)[color getRed:&r green:&g blue:&g alpha:&a];
  view.opaque = a >= 0.999;
}

void uiView_setFrame(UIView* view, CGRect* rect) {
  view.frame = *rect;
}

