{ mkDerivation, stdenv, buildPackages, hostPlatform
, base, monad-control, text, transformers-base }:
mkDerivation {
  pname = "hs-uikit";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base monad-control text transformers-base ];
  libraryFrameworkDepends =
    stdenv.lib.optional (hostPlatform.useiOSPrebuilt)
      "${buildPackages.darwin.xcode}/Contents/Developer/Platforms/${hostPlatform.xcodePlatform}.platform/Developer/SDKs/${hostPlatform.xcodePlatform}.sdk/System";
  homepage = "https://github.com/reflex-frp/reflex-native";
  description = "Bindings to UIKit";
  license = stdenv.lib.licenses.bsd3;
}
