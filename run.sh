#!/bin/bash

if [ "$#" -ge "1" ] ; then
  tests=src/$1.hs
else
  tests=$(ls src/day*.hs)
  echo "Running All Tests..."
  echo "===================="
  echo
fi

for t in $tests ; do
  base=$(basename $t ".hs")
  if [ -f "$t" ] ; then
    echo "$base: "
    if [ -f "inputs/$base.in" ] ; then
      cabal run -v0 "$base" <"inputs/$base.in"
    else
      cabal run -v0 "$base"
    fi
    echo
  fi
done
