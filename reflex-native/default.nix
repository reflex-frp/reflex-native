{ mkDerivation, base, dependent-map, dependent-sum, mtl, ref-tf, reflex, stdenv, text, transformers, vector-space }:
mkDerivation {
  pname = "reflex-native";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base dependent-map dependent-sum mtl ref-tf reflex text transformers vector-space ];
  homepage = "https://github.com/reflex-frp/reflex-native";
  description = "Cross platform layer for developing native Reflex apps";
  license = stdenv.lib.licenses.bsd3;
}
