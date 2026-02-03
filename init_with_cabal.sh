#!/usr/bin/env sh
cabal update &&
    cabal build &&
    printf "#!/usr/bin/env sh\ncabal run -v0 exe:okbgen -- \"\$@\"\n" > run_okbgen &&
    chmod u+x run_okbgen
