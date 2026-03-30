#! /usr/bin/bash

numb=$@
hspath=./Solutions/ex$(echo $numb).hs
binpath=./bin/ex$(echo $numb)/
outpath=$(echo $binpath)ex$(echo $numb)

echo Compiling $hspath
mkdir -p $binpath
ghc $hspath -o $outpath -odir $binpath -hidir $binpath

echo Running $hspath
echo ""
./$outpath