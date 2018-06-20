#import <UIKit/UIKit.h>
#import "HsFFI.h"


CGFloat uiFont_fontWeightFromInt(int weight) {
  switch (weight) {
    case 0: return UIFontWeightUltraLight;
    case 1: return UIFontWeightThin;
    case 2: return UIFontWeightLight;
    case 3: return UIFontWeightRegular;
    case 4: return UIFontWeightMedium;
    case 5: return UIFontWeightSemibold;
    case 6: return UIFontWeightBold;
    case 7: return UIFontWeightHeavy;
    case 8: return UIFontWeightBlack;

    default: return UIFontWeightRegular;
  }
}

UIFont* uiFont_systemFontOfSizeWeight(CGFloat size, int weight) {
  return [UIFont systemFontOfSize:size weight:uiFont_fontWeightFromInt(weight)];
}

UIFont* uiFont_fontWithNameSize(NSString* name, CGFloat size) {
  return [UIFont fontWithName:name size:size];
}
