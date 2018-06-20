{ mkDerivation, stdenv, buildPackages, hostPlatform
, base, monad-control, text, transformers-base }:
mkDerivation {
  pname = "hs-uikit";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base monad-control text transformers-base ];
  libraryDarwinFrameworkDepends = with buildPackages; [ (assert osx_sdk != null; osx_sdk) ];
  homepage = "https://github.com/reflex-frp/reflex-native";
  description = "Bindings to UIKit";
  license = stdenv.lib.licenses.bsd3;
}
