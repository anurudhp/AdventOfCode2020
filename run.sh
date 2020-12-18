#!/bin/bash

if [ "$#" -ge "1" ] ; then
  tests=$1
else
  tests=$(ls day*.hs)
  echo "Running All Tests..."
  echo "===================="
  echo
fi

for t in $tests ; do
  base=$(basename $t ".hs")
  if [ -f "$t" ] ; then
    echo "$base:"
    cabal run -v0 "$base" <"inputs/$base.in"
    echo
  fi
done
