{ mkDerivation, async, attoparsec, base, bytestring, fetchgit
, monad-loops, mtl, network, singletons, stdenv, stm, text
, transformers
}:
mkDerivation {
  pname = "mqtt-hs";
  version = "1.0.1";
  src = fetchgit {
    url = "https://github.com/sorki/mqtt-hs.git";
    sha256 = "0wirp8s2x1iaxr5cisc311f3wnpd08bf0m87c4llmasn815d8q50";
    rev = "c7251dfd18aa6573770a21d6bb65d11f13faa118";
  };
  libraryHaskellDepends = [
    async attoparsec base bytestring monad-loops mtl network singletons
    stm text transformers
  ];
  homepage = "http://github.com/k00mi/mqtt-hs";
  description = "A MQTT client library";
  license = stdenv.lib.licenses.gpl3;
}
