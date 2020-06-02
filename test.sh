#/bin/bash

export PATH=exe

run() {
    echo '--------------------------------------------------'
    echo "$@"
    echo '--------------------------------------------------'
    $@
}
run thrice-thrice.exe
run nfib.exe
run triangle.exe
run fact.exe 6
run nthPrime.exe
run combinator-fact.exe
run list-processing.exe
run over.exe
run pap-over-app.exe

# Need to print string values properly to enable this...
#run pythagorian.exe

exit 0
