#import <UIKit/UIKit.h>
#import "HsFFI.h"

@interface GenericGestureRecognizerTarget : NSObject

@property (nonatomic, assign) HsStablePtr callback;
@property (nonatomic, strong) UIGestureRecognizer* recognizer;

- (instancetype)initWithCallback:(HsStablePtr)callback;

@end

