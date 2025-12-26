generate DAY:
    #!/usr/bin/env bash
    set -euo pipefail
    cat ./template/DayXX.hs | sed -e s/DayXX/Day{{ DAY }}/g > ./src/Day{{ DAY }}.hs
    cat ./template/DayXXSpec.hs | sed -e s/DayXX/Day{{ DAY }}/g | sed -e s/XX.txt/{{ DAY }}.txt/g  > ./test/Day{{ DAY }}Spec.hs
    awk -f - app/Main.hs <<'EOF' > app/Main.hs.new
    /import Protolude/ {
        print "import qualified Day{{ DAY }}"
    }
    /Day not implemented yet/ {
        print "runDay {{ DAY }} = runSolver Day{{ DAY }}.solve \"input/{{ DAY }}.txt\""
    }
    { print }
    EOF
    mv app/Main.hs.new app/Main.hs
    hpack

bench:
  #!/usr/bin/env bash
  set -euo pipefail
  BIN=$(cabal list-bin aoc2025)
  hyperfine --warmup 3 --export-markdown bench.md \
      "$BIN 1" "$BIN 2" "$BIN 3" "$BIN 4" "$BIN 5" "$BIN 6" "$BIN 7" "$BIN 8" "$BIN 9" "$BIN 10" "$BIN 11" "$BIN 12"
