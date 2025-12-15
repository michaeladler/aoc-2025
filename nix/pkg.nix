{ mkDerivation, attoparsec, base, bytestring, containers
, disjoint-containers, hspec, hspec-discover, lib, matrix
, pretty-simple, unordered-containers, vector
}:
mkDerivation {
  pname = "aoc2025";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers disjoint-containers matrix
    pretty-simple unordered-containers vector
  ];
  executableHaskellDepends = [
    attoparsec base bytestring containers disjoint-containers matrix
    pretty-simple unordered-containers vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring containers disjoint-containers hspec
    matrix pretty-simple unordered-containers vector
  ];
  testToolDepends = [ hspec-discover ];
  license = lib.licenses.mit;
  mainProgram = "aoc2025";
}
