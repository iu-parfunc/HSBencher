

import HSBencher
import System.Environment (getEnvironment, getExecutablePath)
import System.Directory   (setCurrentDirectory, getDirectoryContents, getCurrentDirectory)
import System.IO.Unsafe   (unsafePerformIO)
import GHC.Conc           (getNumProcessors)

main = do
  -- Hack to deal with running from cabal:
  rightDir <- fmap ("benchmark.hs" `elem`) $ getDirectoryContents =<< getCurrentDirectory
  if rightDir then return ()
    else do
      path <- getCurrentDirectory
      let hackD = "./example/make_and_ghc"
      putStrLn$"HACK: changing from "++path++" to "++hackD
      setCurrentDirectory hackD 
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
  And [
     -- Set NoMeaning (CompileParam "-threaded -rtsopts")
        Set NoMeaning (RuntimeParam "+RTS -s -qa -RTS")
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
