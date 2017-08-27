#!/bin/bash
for f in Actual/*.hs
do
  echo "=== $f ==="
  stack runghc $f
  echo ""
done
