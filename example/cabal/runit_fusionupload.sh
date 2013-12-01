#!/bin/bash


ghc --make benchmark.hs -o benchmark.run

echo "You must set HSBENCHER_GOOGLE_CLIENTID and HSBENCHER_GOOGLE_CLIENTSECRET env vars for this."
./benchmark.run --fusion-upload --name "Test_FusionUpload" 
