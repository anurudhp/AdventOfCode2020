#!/bin/bash

for i in `seq 1 25` ; do
  if [ -f "day$i.hs" ] ; then
    echo "Day $i:"
    cabal run -v0 "day$i" <inputs/day$i.in
    echo
    echo
  fi
done
