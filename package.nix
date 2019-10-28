{ mkDerivation, aeson, aeson-lens, array, attoparsec, base
, bytestring, containers, hedgehog, http-types, HUnit, io-streams
, json-stream, lens, lens-aeson, stdenv, streaming
, streaming-bytestring, streaming-utils, tasty, tasty-hedgehog
, tasty-hunit, text, text-conversions, wai, warp, webdriver, wreq
}:
mkDerivation {
  pname = "effpee";
  version = "0.1.4";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base bytestring containers http-types lens lens-aeson
    text text-conversions wai
  ];
  executableHaskellDepends = [
    aeson aeson-lens array attoparsec base bytestring containers
    hedgehog http-types HUnit io-streams json-stream lens lens-aeson
    streaming streaming-bytestring streaming-utils tasty tasty-hedgehog
    tasty-hunit text text-conversions wai warp webdriver wreq
  ];
  testHaskellDepends = [
    base hedgehog HUnit tasty tasty-hedgehog tasty-hunit text webdriver
  ];
  homepage = "https://github.com/mbbx6spp/effpee";
  description = "Exercises for teaching typed functional programming";
  license = stdenv.lib.licenses.agpl3;
}
