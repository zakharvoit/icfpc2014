#!/usr/bin/env bash

for f in test*.s ;do
    g=${f%%.s}.ghc
    ./compiler.pl $f | cmp $g
    if [[ $? != 0 ]] ;then
        echo "Error $f"
    else
        echo "Ok $f"
    fi
done
