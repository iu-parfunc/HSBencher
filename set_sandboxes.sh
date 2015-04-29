#!/bin/bash

# Cabal Sandboxes don't work that well with respect to switching GHC versions.
# This is a little helper script that makes it easier to keep SEPARATE sandboxes,
# per GHC version, in separate directories.

set -xe

if [ "$SANDBOX" == "" ]; then
    GHCVER=`ghc --version | grep version | awk '{ print $NF }'`
    SANDBOX=sandbox"$GHCVER"
fi

DIRS=" ./hsbencher/ ./hsbencher-fusion/ ./hsbencher-codespeed ./hsbencher-analytics "

TOP=`pwd`

# Wipe old sandboxes.
# ----------------------------------------

cabal sandbox delete || true
for dir in $DIRS; do
    cd "$dir"
    cabal sandbox delete || true
    cd "$TOP"
done

# Make a new sandbox
# ----------------------------------------

# Take a peek at the GHC version.
which -a ghc
ghc --version

if ! [ -d "$SANDBOX" ]; then
    mkdir "$SANDBOX"
    cd "$SANDBOX"
    cabal sandbox init
    cd "$TOP"
fi

for dir in $DIRS; do
    cd "$dir"
    cabal sandbox init --sandbox="$TOP/$SANDBOX/.cabal-sandbox"
    cd "$TOP"
done

cabal sandbox init --sandbox="$TOP/$SANDBOX/.cabal-sandbox"
cabal sandbox hc-pkg list
