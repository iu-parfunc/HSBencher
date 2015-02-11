

all:
	../.cabal-sandbox/bin/hsbencher-graph --latest=GIT_DEPTH -x ARGS -y MEDIANTIME -k PROGNAME -f get-latest.csv  -o get-latest.out
	grep 0.112864 get-latest.out || (echo "Error: expected get-latest.out to look different."; exit 1)
# Output should contain:
# "ARGS","snowball-list"
# "1000000","0.112864"


clean:
	rm -f get-latest.out
