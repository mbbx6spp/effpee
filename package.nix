{ mkDerivation, aeson, aeson-lens, array, attoparsec, base
, bytestring, containers, generic-deriving, hedgehog, http-types
, HUnit, invariant, io-streams, json-stream, lens, lens-aeson, mtl
, semigroupoids, stdenv, streaming, streaming-bytestring
, streaming-utils, tasty, tasty-hedgehog, tasty-hunit, text
, text-show, time, uuid, wai, warp, webdriver, wreq
}:
mkDerivation {
  pname = "effpee";
  version = "0.1.4";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base bytestring containers generic-deriving http-types
    invariant lens lens-aeson mtl semigroupoids text text-show time
    uuid wai
  ];
  executableHaskellDepends = [
    aeson aeson-lens array attoparsec base bytestring containers
    generic-deriving hedgehog http-types HUnit invariant io-streams
    json-stream lens lens-aeson mtl semigroupoids streaming
    streaming-bytestring streaming-utils tasty tasty-hedgehog
    tasty-hunit text text-show time uuid wai warp webdriver wreq
  ];
  testHaskellDepends = [
    base hedgehog HUnit tasty tasty-hedgehog tasty-hunit text text-show
    webdriver
  ];
  homepage = "https://github.com/mbbx6spp/effpee";
  description = "Exercises for teaching typed functional programming";
  license = stdenv.lib.licenses.agpl3;
}
