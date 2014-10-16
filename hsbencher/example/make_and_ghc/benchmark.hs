

import HSBencher
-- import HSBencher.Backend.Fusion  (defaultFusionPlugin)
import HSBencher.Backend.Dribble (defaultDribblePlugin)

import System.Environment (getEnvironment)
import System.Directory   (setCurrentDirectory, getDirectoryContents, getCurrentDirectory)
import System.IO.Unsafe   (unsafePerformIO)
import GHC.Conc           (getNumProcessors)

main :: IO ()
main = do
  -- Hack to deal with running from cabal:
  rightDir <- fmap ("benchmark.hs" `elem`) $ getDirectoryContents =<< getCurrentDirectory
  if rightDir then return ()
    else do
      path <- getCurrentDirectory
      let hackD = "./example/make_and_ghc"
      putStrLn$"HACK: changing from "++path++" to "++hackD
      setCurrentDirectory hackD 
  defaultMainModifyConfig myconfig


myconfig :: Config -> Config
myconfig cfg@Config{plugIns=p} = 
  cfg { benchlist = benches 
      , plugIns   =
--                    SomePlugin defaultFusionPlugin : 
                    SomePlugin defaultDribblePlugin :
                    p
      } 

benches :: [Benchmark DefaultParamMeaning]
benches =
  [ mkBenchmark "bench1/"          ["unused_cmdline_arg"] envExample
--  , mkBenchmark "bench2/Hello.hs"  []                     withthreads  
  ]

-- No benchmark configuration space.
none :: BenchSpace DefaultParamMeaning
none = And []

envExample :: BenchSpace DefaultParamMeaning
envExample =
  Or [ And [ Set NoMeaning   (CompileParam "-DNOTHREADING")
           , Set (Threads 1) (RuntimeEnv "CILK_NPROCS" "1") ]
     , And [ Set NoMeaning   (CompileParam "-DTHREADING")
           , Or [ Set (Threads 3) (RuntimeEnv "CILK_NPROCS" "3")
                , Set (Threads 4) (RuntimeEnv "CILK_NPROCS" "4")
                ]
           ]
     ]

withthreads :: BenchSpace DefaultParamMeaning
withthreads = defaultHSSettings$
              varyThreads none

defaultHSSettings :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
defaultHSSettings spc =
  And [
        Set NoMeaning (CompileParam "-threaded -rtsopts")
      , Set NoMeaning (RuntimeParam "+RTS -s -qa -RTS")
      , Set NoMeaning (CmdPath      "ghc" "ghc") -- Does nothing.
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
