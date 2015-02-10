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
    $HSG -f snowball-list.csv --GPL \
       --template=./snowball-template.gpl --renames=./renames.txt \
       -x ARGS -y MEDIANTIME --key PROGNAME --key VARIANT --key THREADS $*
}

go -o plot_both.csv

go --filter=VARIANT,cnf     -o plot1.csv
go --filter=VARIANT,normal  -o plot2.csv

gnuplot plot_both.gpl
gnuplot plot1.gpl
gnuplot plot2.gpl
