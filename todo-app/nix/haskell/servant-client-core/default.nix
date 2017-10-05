{ mkDerivation, base, base-compat, base64-bytestring, bytestring
, containers, deepseq, exceptions, generics-sop, hspec
, http-api-data, http-media, http-types, mtl, network-uri
, QuickCheck, safe, servant, stdenv, text, fetchgit
}:
mkDerivation {
  pname = "servant-client-core";
  version = "0.11";
  src = (fetchgit {
    url = "https://github.com/LumiGuide/servant.git";
    sha256 = "1zca0nz1hi8a1vw1az6kaalqaab5gw7z49fkwcxl66rhcd7wh4gk";
    rev = "6afcb09a8fa663de491df5fd07fede814bcb9dab";
  }) + "/servant-client-core";
  libraryHaskellDepends = [
    base base-compat base64-bytestring bytestring containers exceptions
    generics-sop http-api-data http-media http-types mtl network-uri
    safe servant text
  ];
  testHaskellDepends = [ base base-compat deepseq hspec QuickCheck ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "Core functionality and class for client function generation for servant APIs";
  license = stdenv.lib.licenses.bsd3;
}
