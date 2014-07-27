#!/usr/bin/env bash

for f in test*.ghc ;do
    g=${f%%.ghc}.ghc_pure
    ./compiler.pl $f | cmp $g
    if [[ $? != 0 ]] ;then
        echo "Error $f"
    else
        echo "Ok $f"
    fi
done
