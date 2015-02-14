#!/bin/bash

HSG="../.cabal-sandbox/bin/hsbencher-graph"

if [ -e $HSG ]; then
    echo "Using hsbencher-graph executable from sandbox."
else
    HSG="hsbencher-graph"
fi

which $HSG

set -xe

function go() {
    $HSG snowball-list.csv --GPL \
	 --template=./template_both.gpl --renames=./renames.txt \
	 --pad=THREADS,2 \
         -x ARGS -y MEDIANTIME --key PROGNAME --key VARIANT --key THREADS $*
}

go -o plot_both.csv

go --filter=VARIANT,cnf     -o plot1.csv
go --filter=VARIANT,normal  -o plot2.csv

gnuplot plot_both.gpl
gnuplot plot1.gpl
gnuplot plot2.gpl


# We really want: --speedup=VARIANT:cnf-0.9-bugfix-incremental,VARIANT:normal-haskell
# In that syntax, par speedup might be: --speedup=,THREADS:1

# Or maybe: --speedup=VARIANT,cnf-0.9-bugfix-incremental --vs=VARIANT,normal-haskell
