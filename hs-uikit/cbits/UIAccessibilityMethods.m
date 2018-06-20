#import <UIKit/UIAccessibility.h>
#import "HsFFI.h"


void uiAccessibility_setAccessibilityLabel(NSObject* target, NSString* label) {
  target.accessibilityLabel = label;
}
