#!/bin/bash

export PATH=_build

flag=$1 # may be -nn

echo Running tests with flags: $flag

run() {
    prog=$1; shift
    echo '--------------------------------------------------'
    echo $prog "$@"
    echo '--------------------------------------------------'
    $prog$flag "$@"
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
run pythagorian
run parser '12+(3*5)+(2*33)'
run do-example
run json-parser '11'
