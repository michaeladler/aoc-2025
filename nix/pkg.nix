{ mkDerivation, attoparsec, base, bytestring, hspec, hspec-discover
, lib, matrix, text, unordered-containers, vector
}:
mkDerivation {
  pname = "aoc2025";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring matrix text unordered-containers vector
  ];
  executableHaskellDepends = [
    attoparsec base bytestring matrix text unordered-containers vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring hspec matrix text unordered-containers
    vector
  ];
  testToolDepends = [ hspec-discover ];
  license = lib.licenses.mit;
  mainProgram = "aoc2025";
}
