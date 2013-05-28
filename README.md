


See hsbencher.cabal for a general overview.


Protocols for benchmarks to follow
==================================


All benchmarks, via all BuildMethods
------------------------------------

Benchmarks using any BuildMethod, including BuildMethods added by the
end user obey the following conventions:

 * Timing -- complete process runtime is used by default, this can be
   overridden by having the benchmark print out a line such as
   `SELFTIMED 3.3` on stdout , which would indicate a 3.3 second runtime.

 * Coming soon -- compile time timing and multi-phase timing (e.g. for Accelerate) 

Benchmarks using the builtin 'make' BuildMethod
-----------------------------------------------

 * `make` should build the benchmark.
 * `make run` should run the benchmark
 * compile time arguments are provided with `make COMPILE_ARGS='...'`
 * runtime arguments are provided with `make run RUN_ARGS='...'`

Benchmarks using the builtin cabal BuildMethod
----------------------------------------------

 * One .cabal file should be contained in the directory.
 * Either the directory itself, or a file within the directory can be
   the benchmark "target".
 * ONE executable target should be built when `cabal install` or
   `cabal-dev install` is invoked.
 * compile time arguments are formatted for cabal-install
 * runtime arguments are provided to the resulting executable, raw
   (i.e. you need to include `+RTS -RTS` yourself)

Benchmarks using the builtin ghc BuildMethod
--------------------------------------------

 * A single .hs file should be specified as the build target
 * It should build by running `ghc --make` on the target file; any
   include directories beyond the one containing the target file must
   be added explicitly (as CompileParam's)
 * compile time arguments are formatted for ghc command line 
 * runtime arguments are provided to the resulting executable, raw
   (i.e. you need to include `+RTS -RTS` yourself)

