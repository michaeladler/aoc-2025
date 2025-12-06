generate DAY:
    #!/usr/bin/env bash
    set -euo pipefail
    cat ./template/DayXX.hs | sed -e s/DayXX/Day{{ DAY }}/g > ./src/Day{{ DAY }}.hs
    cat ./template/DayXXSpec.hs | sed -e s/DayXX/Day{{ DAY }}/g > ./test/Day{{ DAY }}Spec.hs
    hpack
