{ mkDerivation, aeson, aeson-compat, attoparsec, base, base-compat
, bytestring, Cabal, cabal-doctest, case-insensitive, directory
, doctest, filemanip, filepath, hspec, http-api-data, http-media
, http-types, mmorph, mtl, natural-transformation, network-uri
, QuickCheck, quickcheck-instances, stdenv, string-conversions
, tagged, text, url, vault, fetchgit
}:
mkDerivation {
  pname = "servant";
  version = "0.11";
  src = (fetchgit {
    url = "https://github.com/LumiGuide/servant.git";
    sha256 = "1zca0nz1hi8a1vw1az6kaalqaab5gw7z49fkwcxl66rhcd7wh4gk";
    rev = "6afcb09a8fa663de491df5fd07fede814bcb9dab";
  }) + "/servant";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson attoparsec base base-compat bytestring case-insensitive
    http-api-data http-media http-types mmorph mtl
    natural-transformation network-uri string-conversions tagged text
    vault
  ];
  testHaskellDepends = [
    aeson aeson-compat attoparsec base base-compat bytestring directory
    doctest filemanip filepath hspec QuickCheck quickcheck-instances
    string-conversions text url
  ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "A family of combinators for defining webservices APIs";
  license = stdenv.lib.licenses.bsd3;
}
