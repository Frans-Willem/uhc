#!/bin/bash
ghc UnitTesting.hs
MONO_PATH=../cpslib:$PATH ./UnitTesting ../uhc/EHC/install/8/bin/ehc ../samples
