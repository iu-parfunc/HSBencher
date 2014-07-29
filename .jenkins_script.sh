#!/bin/bash

if [ "$CABAL" == "" ]; then
 CABAL=cabal
fi

PKGS=" ./hsbencher/ ./hsbencher-fusion/ ./hgdata "

$CABAL --version
git submodule update --init
$CABAL sandbox init
$CABAL install --only-dependencies $PKGS -j
$CABAL install $PKGS --enable-tests 
# --show-details=streaming

# cabal install -ffusion --enable-tests ./ http-conduit-1.9.6 handa-gdata-0.6.9.1 --force-reinstalls 
