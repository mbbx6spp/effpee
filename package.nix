{ mkDerivation, array, base, containers, hedgehog, HUnit, stdenv
, tasty, tasty-hedgehog, tasty-hunit, text
}:
mkDerivation {
  pname = "effpee";
  version = "0.1.4";
  src = ./.;
  libraryHaskellDepends = [ array base containers text ];
  testHaskellDepends = [
    base hedgehog HUnit tasty tasty-hedgehog tasty-hunit text
  ];
  homepage = "https://github.com/mbbx6spp/effpee";
  description = "Exercises for teaching typed functional programming";
  license = stdenv.lib.licenses.agpl3;
}
