#/bin/bash

export PATH=_build/exe

run() {
    echo '--------------------------------------------------'
    echo "$@"
    echo '--------------------------------------------------'
    $@
}

run thrice-thrice
run nfib
run triangle
run fact 6
run nthPrime
run combinator-fact
run list-processing
run over
run pap-over-app

# Need to print string values properly to enable this...
#run pythagorian.exe

exit 0
