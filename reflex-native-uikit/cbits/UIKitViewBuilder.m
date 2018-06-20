#import <UIKit/UIKit.h>
#import "HsFFI.h"

UIView* uikitViewBuilder_newHolderView() {
  UIView* v = [[UIView alloc] initWithFrame:CGRectZero];
  v.backgroundColor = [UIColor clearColor];
  v.opaque = NO;
  v.userInteractionEnabled = NO;
  return v;
}

UIView* uikitViewBuilder_newMarkerView() {
  UIView* v = [[UIView alloc] initWithFrame:CGRectZero];
  v.backgroundColor = [UIColor clearColor];
  v.opaque = NO;
  v.userInteractionEnabled = NO;
  return v;
}

UIView* uikitViewBuilder_collectViewsBetween(UIView* start, UIView* end) {
  UIView* holder = uikitViewBuilder_newHolderView();
  UIView* parent = start.superview;
  NSArray* subviews = parent.subviews;
  NSUInteger limit = subviews.count;
  BOOL inRange = NO;
  for (NSUInteger i = 0; i < limit; ++i) {
    UIView* subview = [subviews objectAtIndex:i];
    if (inRange) {
      if (subview == end) {
        return holder;
      } else {
        [holder addSubview:subview];
      }
    } else {
      if (subview == start) {
        inRange = YES;
      }
    }
  }
  return holder;
}

void uikitViewBuilder_deleteViewsBetween(UIView* start, UIView* end) {
  UIView* parent = start.superview;
  NSArray* subviews = parent.subviews;
  NSUInteger limit = subviews.count;
  BOOL inRange = NO;
  for (NSUInteger i = 0; i < limit; ++i) {
    UIView* subview = [subviews objectAtIndex:i];
    if (inRange) {
      if (subview == end) {
        return;
      } else {
        [subview removeFromSuperview];
      }
    } else {
      if (subview == start) {
        inRange = YES;
      }
    }
  }
}

void uikitViewBuilder_replaceBetweenMarkersWithHolder(UIView* start, UIView* end, UIView* holder) {
  uikitViewBuilder_deleteViewsBetween(start, end);
  UIView* parent = start.superview;
  for (UIView* subview in holder.subviews) {
    [parent insertSubview:subview belowSubview:end];
  }
}

void uikitViewBuilder_insertMarkerBeforeMarker(UIView* marker, UIView* beforeMarker) {
  [beforeMarker.superview insertSubview:marker belowSubview:beforeMarker];
}

void uikitViewBuilder_addSubviewsFromHolder(UIView* parent, UIView* holder) {
  for (UIView* subview in holder.subviews) {
    [parent addSubview:subview];
  }
}
