{ mkDerivation, fetchFromGitHub, base, base16-bytestring, binary, bytestring, hspec
, stdenv, text, time, timerep
}:
mkDerivation {
  pname = "cayene-lpp";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "sorki";
    repo = "cayene-lpp";
    rev = "21fda968f065cd04d07364c8d6a9464b4e72b613";
    sha256 = "111fpy8s19mmlanxlz9gv9aflwqjknfaa8fjh8k4swj9fzn6na9p";
  };
  libraryHaskellDepends = [
    base binary bytestring text time timerep
  ];
  testHaskellDepends = [ base base16-bytestring hspec ];
  homepage = "https://github.com/sorki/cayene-lpp";
  description = "Cayenne Low Power Payload";
  license = stdenv.lib.licenses.bsd3;

  doCheck = false; # ghcjs issues
}
