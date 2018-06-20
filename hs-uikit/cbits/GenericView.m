#import "GenericView.h"
#include "UIKit/Generic/View_stub.h"

@implementation GenericView

- (id)initWithCallback:(HsStablePtr)callback {
  if (!(self = [super init])) {
    return self;
  }

  _config = genericViewImpl_initialize((__bridge HsPtr)self, callback);

  return self;
}

- (void)dealloc {
  genericViewImpl_release(_config);
}

- (void)drawRect:(CGRect)rect {
  genericViewImpl_drawRect(_config, &rect);
}

@end

UIView* genericView_new(HsStablePtr callback) {
  return [[GenericView alloc] initWithCallback:callback];
}

