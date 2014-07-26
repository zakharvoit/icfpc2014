#!/usr/bin/env bash

for f in test*.lisp ;do
    g=${f%%.lisp}.gcc
    ./compiler <$f | cmp $g
    if [[ $? != 0 ]] ;then
        echo "Error $f"
    else
        echo "Ok $f"
    fi
done
