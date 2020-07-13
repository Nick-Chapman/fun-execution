#!/bin/bash

if [ $# != 2 ]; then echo "$0 : expected 2 args, got $#"; fi

sloppy() {
    cat - # be precise!
    #cat - | sed 's/heap used, .* cells/heap used, ? cells/'
    #cat - | sed 's/#steps = .*/#steps = ?/'
}

diff <(cat $1|sloppy) <(cat $2|sloppy)
