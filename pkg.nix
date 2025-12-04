{ mkDerivation, attoparsec, base, hspec, hspec-discover, lib
, matrix, text
}:
mkDerivation {
  pname = "aoc2025";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ attoparsec base matrix text ];
  executableHaskellDepends = [ attoparsec base matrix text ];
  testHaskellDepends = [ attoparsec base hspec matrix text ];
  testToolDepends = [ hspec-discover ];
  license = lib.licenses.mit;
  mainProgram = "aoc2025";
}
