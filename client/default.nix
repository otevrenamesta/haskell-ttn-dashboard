{ stdenv, lib, mkDerivation, base, miso, aeson, servant, network-uri, text, time, shakespeare, ttn, cayene-lpp
, patchWsURL ? null }:
mkDerivation {
  pname = "haskell-ttn-dashboard-client";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso aeson servant network-uri text time shakespeare ttn cayene-lpp ];
  description = "TTN dashboard web";
  license = stdenv.lib.licenses.bsd3;

  postPatch = lib.optionalString (patchWsURL != null) ''
    substituteInPlace client/Main.hs \
      --replace 'URL "ws://127.0.0.1:8000"' 'URL "${patchWsURL}"'
  '';
}
