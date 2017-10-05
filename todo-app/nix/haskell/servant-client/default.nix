{ mkDerivation, aeson, attoparsec, base, base-compat, bytestring
, containers, deepseq, exceptions, generics-sop, hspec
, http-api-data, http-client, http-client-tls, http-media
, http-types, HUnit, monad-control, mtl, network, QuickCheck
, semigroupoids, servant, servant-client-core, servant-server
, stdenv, text, transformers, transformers-base
, transformers-compat, wai, warp, fetchgit
}:
mkDerivation {
  pname = "servant-client";
  version = "0.11";
  src = (fetchgit {
    url = "https://github.com/LumiGuide/servant.git";
    sha256 = "1zca0nz1hi8a1vw1az6kaalqaab5gw7z49fkwcxl66rhcd7wh4gk";
    rev = "6afcb09a8fa663de491df5fd07fede814bcb9dab";
  }) + "/servant-client";
  libraryHaskellDepends = [
    aeson attoparsec base base-compat bytestring containers exceptions
    http-client http-client-tls http-media http-types monad-control mtl
    semigroupoids servant-client-core text transformers
    transformers-base transformers-compat
  ];
  testHaskellDepends = [
    aeson base base-compat bytestring containers deepseq generics-sop
    hspec http-api-data http-client http-media http-types HUnit mtl
    network QuickCheck servant servant-client-core servant-server text
    transformers transformers-compat wai warp
  ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "automatical derivation of querying functions for servant webservices";
  license = stdenv.lib.licenses.bsd3;
}
