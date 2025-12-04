ALL_FILES := aoc2025.cabal nix/pkg.nix

all: $(ALL_FILES)

.PHONY: aoc2025.cabal
aoc2025.cabal:
	hpack

nix/pkg.nix: aoc2025.cabal
	cabal2nix ./. >$@

.PHONY: clean
clean:
	$(RM) $(ALL_FILES)
