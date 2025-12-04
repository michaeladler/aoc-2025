{ mkDerivation, attoparsec, base, hspec, hspec-discover, lib, text
, unordered-containers
}:
mkDerivation {
  pname = "aoc2025";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base text unordered-containers
  ];
  executableHaskellDepends = [
    attoparsec base text unordered-containers
  ];
  testHaskellDepends = [
    attoparsec base hspec text unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  license = lib.licenses.mit;
  mainProgram = "aoc2025";
}
