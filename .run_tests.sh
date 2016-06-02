#!/bin/bash

# Run all the tests.
# Uses env var:
#  - STACK_RESOLVER

$STACK test hsbencher-codespeed
$STACK test hsbencher-fusion
$STACK test hsbencher-graph

$STACK test hsbencher:hsbencher-unit-tests
$STACK test hsbencher:hsbencher-test2
$STACK test hsbencher:hsbencher-test3
 
 # [2016.05.31] Problems with calling cabal as subprocess:
 # And no good way to avoid it, even with cabal-1.24:
 # 
 #    cabal: Use of GHC's environment variable GHC_PACKAGE_PATH is
 #    incompatible with Cabal
 # 
 #$STACK test hsbencher:hsbencher-test1


# Here's a work-around:
# FIXME: Need cabal in path for this:
#TESTEXE=`find -name hsbencher-test1 -type f`
#$TESTEXE
