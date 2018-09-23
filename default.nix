{ mkDerivation, aeson, aeson-pretty, attoparsec, base, beam-core
, beam-postgres, bytestring, containers, directory, lens
, monad-logger, monad-time, mtl, postgresql-simple, safe-money
, servant-server, stdenv, stm, text, time, time-exts, transformers
, wai, wai-logger, warp
}:
mkDerivation {
  pname = "happy-hour-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty attoparsec base beam-core beam-postgres
    bytestring containers directory lens monad-logger monad-time mtl
    postgresql-simple safe-money servant-server stm text time time-exts
    transformers wai wai-logger warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
