{ mkDerivation, attoparsec, base, hspec, hspec-discover, lib, text
}:
mkDerivation {
  pname = "aoc2025";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ attoparsec base text ];
  executableHaskellDepends = [ attoparsec base text ];
  testHaskellDepends = [ attoparsec base hspec text ];
  testToolDepends = [ hspec-discover ];
  license = lib.licenses.mit;
  mainProgram = "aoc2025";
}
