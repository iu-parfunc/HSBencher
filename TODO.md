

[2013.05.27] .. [2013.05.28]

 * BUG: Env vars aren't getting through with the Make method.

 * On mac OS the productivities look like they're being read back in wrong.
 * prune suffixes, while retaining uniqueness
 
 * restore parallelism
   * do path-based locking

 * Add CPU load sanity check to fusion table schema

 * fix non-keepgoing option to actually die on benchmark failure (not just compile)

Longer term:

 * enable idempotent benchmarking + ctrl-C (i.e. fill in the holes)
 
 ? move over to bytestring IO completely
 
 * Further enable packing up a benchmark set to run on a machine
   without GHC (as with Haskell Cnc).
