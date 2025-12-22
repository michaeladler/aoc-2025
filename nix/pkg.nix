{ mkDerivation, array, attoparsec, base, bytestring, containers
, directory, disjoint-containers, hspec, hspec-discover, lib
, pretty-simple, protolude, unordered-containers, vector, z3
}:
mkDerivation {
  pname = "aoc2025";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array attoparsec base bytestring containers directory
    disjoint-containers pretty-simple protolude unordered-containers
    vector z3
  ];
  executableHaskellDepends = [
    array attoparsec base bytestring containers directory
    disjoint-containers pretty-simple protolude unordered-containers
    vector z3
  ];
  testHaskellDepends = [
    array attoparsec base bytestring containers directory
    disjoint-containers hspec pretty-simple protolude
    unordered-containers vector z3
  ];
  testToolDepends = [ hspec-discover ];
  license = lib.licenses.mit;
  mainProgram = "aoc2025";
}
