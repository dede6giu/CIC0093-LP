#! /usr/bin/bash

numb=$@
hspath=./Solutions/ex$(echo $numb).hs
binpath=./bin/ex$(echo numb)/
outpath=$(echo $binpath)ex$(echo $numb)

echo Running $hspath
echo ""
./$outpath