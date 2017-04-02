#!/bin/bash
CURDIR=$(pwd)
cd ../cpslib
./compile.sh
cd $CURDIR
ghc UnitTesting.hs
MONO_PATH=../cpslib:$PATH ./UnitTesting ../../EHC/install/8/bin/ehc ../samples
