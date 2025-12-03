{ mkDerivation, attoparsec, base, containers, hspec, hspec-discover
, lib, text
}:
mkDerivation {
  pname = "aoc2025";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ attoparsec base containers text ];
  executableHaskellDepends = [ attoparsec base containers text ];
  testHaskellDepends = [ attoparsec base containers hspec text ];
  testToolDepends = [ hspec-discover ];
  license = lib.licenses.mit;
  mainProgram = "aoc2025";
}
