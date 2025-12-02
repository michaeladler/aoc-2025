
ALL_FILES := aoc2025.cabal pkg.nix

all: $(ALL_FILES)

aoc2025.cabal: package.yaml
	hpack

pkg.nix: aoc2025.cabal
	cabal2nix ./. >$@

.PHONY: clean
clean:
	$(RM) $(ALL_FILES)
