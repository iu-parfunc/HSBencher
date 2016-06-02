#!/bin/bash

set -xe

# Build everything using "stack", the modern way!
# ------------------------------------------------

which -a stack || echo ok

if [ "$STACK_RESOLVER" == lts-2.22 ]; then 
    export STACK_YAML=stack-lts-2.22.yaml
fi

source .build_tests.sh 
source .run_tests.sh
