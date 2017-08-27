#!/bin/bash
for m4file in Actual/*.m4
do
  # strip .m4 extension and change to .hs
  hsfile="${m4file%.*}.hs"
  m4 -d $m4file > $hsfile
done
