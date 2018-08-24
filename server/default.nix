{ mkDerivation, aeson, base, containers, http-types, lucid, miso
, mtl, network-uri, servant, servant-lucid, servant-server, stdenv
, text, time, wai, wai-app-static, wai-extra, warp
, base64-bytestring, bytestring, stm
, websockets
, shakespeare
, ttn
, ttn-client
, cayene-lpp
, binary
}:
mkDerivation {
  pname = "haskell-ttn-dashboard-server";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers http-types lucid miso mtl network-uri servant
    servant-lucid servant-server text time wai wai-app-static wai-extra warp
    ttn ttn-client websockets shakespeare cayene-lpp binary
    base64-bytestring bytestring stm
  ];
  homepage = "https://github.com/sorki/haskell-ttn-dashboard";
  license = stdenv.lib.licenses.bsd3;
}
