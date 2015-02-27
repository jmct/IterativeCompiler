#!/bin/bash

flags="-I50"
os="20 40 80 160 320"
cs="1 2 4 8 16 24"

for flag in $flags; do
  for o in $os; do
    for c in $cs; do
        echo $o $c
        cd run-$flag-$o-$c
        wc -l $1.0 | cut -d' ' -f1 | sed -e "s/^/$c /" >> ../$o.runtimes 
        cd ..
    done
  done
done
