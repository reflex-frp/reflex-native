#import <Foundation/Foundation.h>
#import "HsFFI.h"

NSString* nsString_stringWithCharactersLength(const unichar* chars, unsigned long len) {
  return [NSString stringWithCharacters:chars length:len];
}

unsigned long nsString_length(NSString* nss) {
  return nss.length;
}

void nsString_getCharacters(NSString* nss, unichar* chars) {
  [nss getCharacters:chars range:NSMakeRange(0, nss.length)];
}

