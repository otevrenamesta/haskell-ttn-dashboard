{ mkDerivation, fetchFromGitHub, aeson, base, bytestring, hspec, hspec-expectations
, raw-strings-qq, stdenv, text, time, timerep
}:
mkDerivation {
  pname = "ttn";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "sorki";
    repo = "data-ttn";
    rev = "8ea4695b575d16a4a1cc29d4606c779339b87f6";
    sha256 = "03hcri0d5aixkbhf0jn5f9kxvnzh9mm9d3vgga5c0jlbjwydb0a8";
  };

  libraryHaskellDepends = [
    aeson base bytestring text time timerep
  ];
  testHaskellDepends = [
    aeson base hspec hspec-expectations raw-strings-qq
  ];

  homepage = "https://github.com/sorki/data-ttn";
  description = "Things Tracker Network JSON Types";
  license = stdenv.lib.licenses.bsd3;

  doCheck = false; # ghcjs issues
}
