#!/usr/bin/sh
cabal update &&
    cabal build &&
    printf "#!/usr/bin/sh\ncabal run -v0 exe:okbgen\n" > run_okbgen.sh &&
    chmod u+x run_okbgen.sh
