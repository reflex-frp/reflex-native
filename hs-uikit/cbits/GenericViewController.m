#import "GenericViewController.h"
#import "UIKit/Generic/ViewController_stub.h"

@implementation GenericViewController

- (id)initWithCallback:(HsStablePtr)callback {
  if (!(self = [super init])) {
    return self;
  }

  _config = genericViewController_initialize((__bridge HsPtr)self, callback);

  return self;
}

- (void)dealloc {
  genericViewController_release(_config);
}

- (void)loadView {
  [super loadView];
  self.view = (__bridge_transfer UIView*)genericViewController_loadView(_config);
}

- (void)viewDidLoad {
  [super viewDidLoad];
  genericViewController_viewDidLoad(_config);
}

- (void)didReceiveMemoryWarning {
  [super didReceiveMemoryWarning];
  genericViewController_didReceiveMemoryWarning(_config);
}

@end

UIViewController* genericViewController_new(HsStablePtr callback) {
  return [[GenericViewController alloc] initWithCallback:callback];
}
