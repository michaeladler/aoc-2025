{ mkDerivation, attoparsec, base, hspec, hspec-discover, lib
, matrix, text, unordered-containers
}:
mkDerivation {
  pname = "aoc2025";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base matrix text unordered-containers
  ];
  executableHaskellDepends = [
    attoparsec base matrix text unordered-containers
  ];
  testHaskellDepends = [
    attoparsec base hspec matrix text unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  license = lib.licenses.mit;
  mainProgram = "aoc2025";
}
