{ mkDerivation, base, containers, dependent-map, dependent-sum, exception-transformers, hs-uikit, mtl, primitive, ref-tf, reflex, reflex-native, stdenv, transformers }:
mkDerivation {
  pname = "reflex-native-uikit";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers dependent-map dependent-sum exception-transformers hs-uikit mtl primitive ref-tf reflex reflex-native transformers ];
  homepage = "https://github.com/reflex-frp/reflex-native";
  description = "Reflex FRP using UIKit directly, via hs-uikit";
  license = stdenv.lib.licenses.bsd3;
}
