#!/bin/bash


ghc --make benchmark.hs -o benchmark.run
./benchmark.run
