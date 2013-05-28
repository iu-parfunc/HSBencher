

import HSBencher
import System.Environment (getEnvironment)
import System.IO.Unsafe   (unsafePerformIO)
import GHC.Conc           (getNumProcessors)

main =
  defaultMainWithBechmarks benches

benches =
  [ Benchmark2 "bench1/" ["unused_cmdline_arg"] withthreads
  ]

withthreads = defaultHSSettings$
              varyThreads (And[])

defaultHSSettings spc =
  And [ Set NoMeaning (CompileParam "--ghc-option='-threaded' --ghc-option='-rtsopts'")
      , Set NoMeaning (RuntimeParam "+RTS -s -qa -RTS")
      , spc]

varyThreads :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
varyThreads conf = And [ conf, Or (map fn threadSelection) ]
 where
   fn n = Set (Threads n) $ RuntimeParam ("+RTS -N"++ show n++" -RTS")

threadSelection :: [Int]
threadSelection = unsafePerformIO $ do
  env <- getEnvironment
  p   <- getNumProcessors
  return [1 .. p]
