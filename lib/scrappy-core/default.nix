{ mkDerivation, aeson, base, bytestring, containers, directory
, filepath, hedgehog, henforcer, lens, lib, modern-uri, network-uri
, parsec, parser-combinators, tasty, tasty-hedgehog, text
, transformers
}:
mkDerivation {
  pname = "scrappy-core";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers directory filepath henforcer lens
    modern-uri network-uri parsec parser-combinators text transformers
  ];
  testHaskellDepends = [
    aeson base bytestring containers directory filepath hedgehog lens
    modern-uri parsec tasty tasty-hedgehog text transformers
  ];
  homepage = "https://github.com/TypifyDev/scrappy";
  description = "html pattern matching library and high-level interface concurrent requests lib for webscraping";
  license = lib.licenses.bsd3;
}
