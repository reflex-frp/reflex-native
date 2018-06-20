#include "HsFFI.h"
#import <UIKit/UIKit.h>

@interface GenericViewController : UIViewController

@property (nonatomic, assign) HsStablePtr config;

- (instancetype)initWithCallback:(HsStablePtr)callback;

@end
