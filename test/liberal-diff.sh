#!/bin/bash

if [ $# != 2 ]; then echo "$0 : expected 2 args, got $#"; fi

sloppy() {
    cat - \
        | sed 's/#steps = .*/#steps = ?/' \
        | sed 's/heap used, .* cells/heap used, ? cells/' \
        | cat
}

diff <(cat $1|sloppy) <(cat $2|sloppy)
