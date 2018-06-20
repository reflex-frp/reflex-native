#include "HsFFI.h"
#import <UIKit/UIKit.h>

@interface GenericView : UIView

@property (nonatomic, assign) HsStablePtr config;

- (instancetype)initWithCallback:(HsStablePtr) callback;

@end

