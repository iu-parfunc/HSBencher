#!/bin/bash

if [ "$CABAL" == "" ]; then
 CABAL=cabal
fi

$CABAL --version
git submodule update --init
$CABAL sandbox init
$CABAL install --only-dependencies ./hsbencher/ ./hsbencher-fusion/ -j
$CABAL install ./hsbencher/ ./hsbencher-fusion/ --enable-tests 
# --show-details=streaming

# cabal install -ffusion --enable-tests ./ http-conduit-1.9.6 handa-gdata-0.6.9.1 --force-reinstalls 
