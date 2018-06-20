#import <UIKit/UIKit.h>
#import "HsFFI.h"

UILabel* uiLabel_new() {
  return [UILabel new];
}

void uiLabel_setFont(UILabel*__nonnull label, UIFont*__nullable font) {
  label.font = font;
}

void uiLabel_setText(UILabel*__nonnull label, NSString*__nullable text) {
  label.text = text;
}

void uiLabel_setTextColor(UILabel*__nonnull label, UIColor*__nullable color) {
  label.textColor = color;
}

