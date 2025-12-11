{ mkDerivation, attoparsec, base, bytestring, disjoint-containers
, hspec, hspec-discover, lib, matrix, unordered-containers, vector
}:
mkDerivation {
  pname = "aoc2025";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring disjoint-containers matrix
    unordered-containers vector
  ];
  executableHaskellDepends = [
    attoparsec base bytestring disjoint-containers matrix
    unordered-containers vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring disjoint-containers hspec matrix
    unordered-containers vector
  ];
  testToolDepends = [ hspec-discover ];
  license = lib.licenses.mit;
  mainProgram = "aoc2025";
}
