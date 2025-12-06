nix/pkg.nix: aoc2025.cabal
	cabal2nix ./. >$@
