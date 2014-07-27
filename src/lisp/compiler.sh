#!/usr/bin/env bash

cat $@ | ../haskell/compiler/compiler | perl -lne 'chomp; print $_ . " ;;; " . $cnt++ if ($_ ne "")'
