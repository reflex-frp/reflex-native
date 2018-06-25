{ mkDerivation, stdenv
, base, reflex, reflex-native, reflex-native-uikit ? null, vector-space }:
mkDerivation {
  pname = "reflex-native";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base reflex reflex-native reflex-native-uikit vector-space ];
  homepage = "https://github.com/reflex-frp/reflex-native";
  description = "Cross platform layer for developing native Reflex apps";
  license = stdenv.lib.licenses.bsd3;
}

