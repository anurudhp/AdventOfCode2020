#!/bin/bash

if [ "$#" -ge "1" ] ; then
  tests=$1
else
  tests=`seq 1 25`
  echo "Running All Tests..."
  echo "===================="
  echo
fi

for i in $tests ; do
  if [ -f "day$i.hs" ] ; then
    echo "Day $i:"
    cabal run -v0 "day$i" <inputs/day$i.in
    echo
  fi
done
