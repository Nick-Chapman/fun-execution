#/bin/bash

export PATH=_build/exe

run() {
    echo '--------------------------------------------------'
    echo "$@"
    echo '--------------------------------------------------'
    $@
}

run fact 6
run triangle 100
run nfib 10
run nthPrime 10

run thrice-thrice
run combinator-fact
run list-processing
run over
run pap-over-app

# Need to print string values properly to enable this...
#run pythagorian.exe

exit 0
