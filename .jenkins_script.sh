#!/bin/bash

# Build everything using "stack", the modern way!
# ------------------------------------------------

set -xe

which -a stack
stack --version
# stack --no-system-ghc build
stack --no-system-ghc test

# TODO: bring back these other tests:

# # Next, build individual tests/examples that we can't actually run,
# # because we don't want to connect to the network and upload data:
# mkdir -p ./bin
# $CABAL install --bindir=./bin ./hsbencher/example/custom_tag
# # $CABAL exec custom-tag

# $CABAL install --bindir=./bin ./hsbencher-fusion/examples/fusion_backend/

# $CABAL install --bindir=./bin ./hsbencher-codespeed/example/

# $CABAL install --bindir=./bin ./hsbencher-analytics/examples/fusion-analytics/
