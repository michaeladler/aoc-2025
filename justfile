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
        print "runDay {{ DAY }} = solveDay Day{{ DAY }}.solve \"input/{{ DAY }}.txt\""
    }
    { print }
    EOF
    mv app/Main.hs.new app/Main.hs
    hpack
