#!/bin/bash

set -xe 

if [ "$CABAL" == "" ]; then
 CABAL=cabal
fi

PKGS=" ./hsbencher/ ./hsbencher-fusion/ ./hgdata "

$CABAL --version
git submodule update --init
$CABAL sandbox init
$CABAL install $PKGS -j --run-tests

# Next, build individual tests/examples that we can't actually run,
# because we don't want to connect to the network and upload data:

