{ mkDerivation, aeson, attoparsec, base, base-compat
, base64-bytestring, bytestring, Cabal, cabal-doctest, containers
, directory, doctest, exceptions, filemanip, filepath, hspec
, hspec-wai, http-api-data, http-types, monad-control, mtl, network
, network-uri, parsec, QuickCheck, resourcet, safe, servant
, should-not-typecheck, split, stdenv, string-conversions
, system-filepath, tagged, temporary, text, transformers
, transformers-base, transformers-compat, wai, wai-app-static
, wai-extra, warp, word8, fetchgit
}:
mkDerivation {
  pname = "servant-server";
  version = "0.11";
  src = (fetchgit {
    url = "https://github.com/LumiGuide/servant.git";
    sha256 = "1zca0nz1hi8a1vw1az6kaalqaab5gw7z49fkwcxl66rhcd7wh4gk";
    rev = "6afcb09a8fa663de491df5fd07fede814bcb9dab";
  }) + "/servant-server";
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson attoparsec base base-compat base64-bytestring bytestring
    containers exceptions filepath http-api-data http-types
    monad-control mtl network network-uri resourcet safe servant split
    string-conversions system-filepath tagged text transformers
    transformers-base transformers-compat wai wai-app-static warp word8
  ];
  executableHaskellDepends = [ aeson base servant text wai warp ];
  testHaskellDepends = [
    aeson base base-compat base64-bytestring bytestring directory
    doctest exceptions filemanip filepath hspec hspec-wai http-types
    mtl network parsec QuickCheck resourcet safe servant
    should-not-typecheck string-conversions temporary text transformers
    transformers-compat wai wai-extra warp
  ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "A family of combinators for defining webservices APIs and serving them";
  license = stdenv.lib.licenses.bsd3;
}
