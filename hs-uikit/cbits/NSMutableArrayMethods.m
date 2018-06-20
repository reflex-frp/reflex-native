#import <Foundation/Foundation.h>
#import "HsFFI.h"

NSMutableArray* nsMutableArray_new() {
  return [NSMutableArray new];
}

void nsMutableArray_addObject(NSMutableArray* ma, id obj) {
  [ma addObject:obj];
}

void nsMutableArray_addObjectsFromArray(NSMutableArray* ma, NSArray* a) {
  [ma addObjectsFromArray:a];
}

