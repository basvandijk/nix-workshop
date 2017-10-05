{ mkDerivation, base, bytestring, case-insensitive, containers
, exceptions, ghcjs-base, ghcjs-prim, http-types, monad-control
, mtl, semigroupoids, servant-client-core, stdenv
, string-conversions, transformers, transformers-base, fetchgit
}:
mkDerivation {
  pname = "servant-client-ghcjs";
  version = "0.11";
  src = (fetchgit {
    url = "https://github.com/LumiGuide/servant.git";
    sha256 = "1zca0nz1hi8a1vw1az6kaalqaab5gw7z49fkwcxl66rhcd7wh4gk";
    rev = "6afcb09a8fa663de491df5fd07fede814bcb9dab";
  }) + "/servant-client-ghcjs";
  libraryHaskellDepends = [
    base bytestring case-insensitive containers exceptions ghcjs-base
    ghcjs-prim http-types monad-control mtl semigroupoids
    servant-client-core string-conversions transformers
    transformers-base
  ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "automatical derivation of querying functions for servant webservices for ghcjs";
  license = stdenv.lib.licenses.bsd3;
}
