#!/bin/bash

# Make sure GHC is installed, and perform any needed mutation of the
# stack yaml.

# Uses env vars:
#  - STACK_RESOLVER

# Args to always use:
STACK_ARGS=" --no-terminal --no-system-ghc "

if [ "$STACK_RESOLVER" == "" ];
then STACK="stack $STACK_ARGS ";
else STACK="stack $STACK_ARGS --resolver=$STACK_RESOLVER";
fi

$STACK setup 

if [ "$STACK_RESOLVER" == ghc-7.8  ] ||
   [ "$STACK_RESOLVER" == ghc-7.10 ] ||
   [ "$STACK_RESOLVER" == ghc-8.0  ] ||
   [ "$STACK_RESOLVER" == nightly  ] ; 
then stack setup ;
     stack --no-terminal --install-ghc install cabal-install ;
     cabal --version ;
     $STACK solver --update-config ;
fi

$STACK exec ghc -- --version                     || echo '?'
$STACK exec ghc -- --print-project-git-commit-id || echo '?'
$STACK build --test --no-run-tests
