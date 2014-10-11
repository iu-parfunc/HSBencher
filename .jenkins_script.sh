#!/bin/bash

set -xe 

if [ "$CABAL" == "" ]; then
 CABAL=cabal
fi

PKGS=" ./hsbencher/ ./hsbencher-fusion/ ./hsbencher-codespeed ./hsbencher-analytics ./hgdata "

$CABAL --version
git submodule update --init
$CABAL sandbox init
$CABAL install $PKGS -j --run-tests

# Next, build individual tests/examples that we can't actually run,
# because we don't want to connect to the network and upload data:
mkdir -p ./bin
$CABAL install --bindir=./bin ./hsbencher/example/custom_tag
# $CABAL exec custom-tag

$CABAL install --bindir=./bin ./hsbencher-fusion/examples/fusion_backend/

$CABAL install --bindir=./bin ./hsbencher-codespeed/example/

$CABAL install --bindir=./bin ./hsbencher-analytics/examples/fusion-analytics/
