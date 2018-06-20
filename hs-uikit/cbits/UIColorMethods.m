#import <UIKit/UIKit.h>
#import "HsFFI.h"

UIColor* uiColor_colorWithRedGreenBlueAlpha(CGFloat red, CGFloat green, CGFloat blue, CGFloat alpha) {
  return [UIColor colorWithRed:red green:green blue:blue alpha:alpha];
}
