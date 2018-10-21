{ mkDerivation, base, bytestring, case-insensitive, containers
, exceptions, ghcjs-base, ghcjs-prim, http-media, http-types
, monad-control, mtl, semigroupoids, servant-client-core, stdenv
, string-conversions, transformers, transformers-base
}:
mkDerivation {
  pname = "servant-client-ghcjs";
  version = "0.14";
  src = ./servant-client-ghcjs;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers exceptions ghcjs-base
    ghcjs-prim http-media http-types monad-control mtl semigroupoids
    servant-client-core string-conversions transformers
    transformers-base
  ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "automatical derivation of querying functions for servant webservices for ghcjs";
  license = stdenv.lib.licenses.bsd3;
}
