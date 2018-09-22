{ mkDerivation, aeson, aeson-pretty, attoparsec, base, beam-core
, beam-postgres, beam-sqlite, bytestring, containers, directory
, lens, mtl, safe-money, servant-server, sqlite-simple, stdenv, stm
, text, time, wai, warp
}:
mkDerivation {
  pname = "happy-hour-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty attoparsec base beam-core beam-postgres
    beam-sqlite bytestring containers directory lens mtl safe-money
    servant-server sqlite-simple stm text time wai warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
