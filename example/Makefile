

# TEMP: Fix the dependency on ntimes scripts:
all: benchmark.run
	cp ../bench/ntimes_minmedmax . 
	cp ../bench/ntimes . 
	./benchmark.run

benchmark.run:
	ghc ../bench/benchmark.hs -o benchmark.run

clean:

distclean:
	rm -rf benchmark.run *.bak *.dat *.log ./bin/ ./build/ ./runs/
