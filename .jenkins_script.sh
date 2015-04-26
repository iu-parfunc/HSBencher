#!/bin/bash

set -xe

if [ "$CABAL" == "" ]; then
 CABAL=cabal-1.22
fi

PKGS=" ./hsbencher/ ./hsbencher-fusion/ ./hsbencher-codespeed ./hsbencher-analytics ./hgdata "

$CABAL --version
TOP=`pwd`

which -a ghc
ghc --version

if [ "NOSETUP" != "1" ]; then
  git submodule update --init
  $CABAL sandbox init
  # for pkg in $PKGS; do
  #     cd "$pkg"
  #     $CABAL sandbox init --sandbox=../.cabal-sandbox/
  #     cd "$TOP"
  # done
fi

# "--run-tests" Requires cabal 1.20+
$CABAL install $PKGS -j --run-tests
# $CABAL install $PKGS -j --enable-tests

# cd "./hsbencher/"
# $CABAL test
# cd "$TOP"

# Next, build individual tests/examples that we can't actually run,
# because we don't want to connect to the network and upload data:
mkdir -p ./bin
$CABAL install --bindir=./bin ./hsbencher/example/custom_tag
# $CABAL exec custom-tag

$CABAL install --bindir=./bin ./hsbencher-fusion/examples/fusion_backend/

$CABAL install --bindir=./bin ./hsbencher-codespeed/example/

$CABAL install --bindir=./bin ./hsbencher-analytics/examples/fusion-analytics/
