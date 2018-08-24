{ mkDerivation, fetchFromGitHub, aeson, base, base64-bytestring, binary, bytestring
, cayene-lpp, config-ini, directory, filepath, monad-logger
, mqtt-hs, pretty-simple, stdenv, stm, text, ttn
}:
mkDerivation {
  pname = "ttn-client";
  version = "0.1.0.1";
  src = fetchFromGitHub {
    owner = "sorki";
    repo = "ttn-client";
    rev = "87c979d2818944fa0dc6115dd75d6362a68980ed";
    sha256 = "14w6w5dxsi983zvc2p25mhr5psykbrqj3rwk7y9v87v86hhj27qd";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring binary bytestring config-ini directory
    filepath monad-logger mqtt-hs pretty-simple stm text ttn
  ];
  executableHaskellDepends = [
    aeson base binary bytestring cayene-lpp mqtt-hs pretty-simple stm
    text ttn
  ];
  homepage = "https://github.com/sorki/ttn-client";
  description = "TheThingsNetwork client";
  license = stdenv.lib.licenses.bsd3;
}
