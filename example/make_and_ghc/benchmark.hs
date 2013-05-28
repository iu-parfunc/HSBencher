

import HSBencher
import System.Environment (getEnvironment)
import System.IO.Unsafe   (unsafePerformIO)
import GHC.Conc           (getNumProcessors)

main =
  defaultMainWithBechmarks benches

benches =
  [ Benchmark2 "bench1/"          ["unused_cmdline_arg"] none
  , Benchmark2 "bench2/Hello.hs"  []                     withthreads
  ]

-- No benchmark configuration space.
none = And []

withthreads = defaultHSSettings$
              varyThreads none

defaultHSSettings spc =
  And [ Set NoMeaning (CompileParam "-threaded" "")
      , Set NoMeaning (RuntimeParam "+RTS -s -qa -RTS" "")
      , spc]

varyThreads :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
varyThreads conf = And [ conf, Or (map fn threadSelection) ]
 where
   fn n = Set (Threads n) $ RuntimeParam "" ("+RTS -N"++ show n++" -RTS")

threadSelection :: [Int]
threadSelection = unsafePerformIO $ do
  env <- getEnvironment
  p   <- getNumProcessors
  return [1 .. p]
