#!/usr/bin/env runghc
{-# LANGUAGE NamedFieldPuns #-}

import System.Directory
import System.FilePath
import System.Exit
import System.Environment (getArgs)

import GHC.Conc (getNumProcessors)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Char8 as B
import Data.Monoid 

import HSBencher
import HSBencher.Internal.MeasureProcess
import HSBencher.Backend.Dribble

import Prelude hiding (log)
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainModifyConfig myconf

all_benchmarks :: [Benchmark DefaultParamMeaning]
all_benchmarks =
  [  mkBenchmark "Bench/Bench.cabal" [] defaultCfgSpc ]
  
-- | Default configuration space over which to vary settings:
--   This is a combination of And/Or boolean operations, with the ability
--   to set various environment and compile options.
defaultCfgSpc = And []

-- Interface needs some work
customHarvester = taggedLineHarvester (B.pack "MYTAG") (\d r -> r {custom=[("MYTAG",IntResult d)]})

-- | Here we have the option of changing the HSBencher config
myconf :: Config -> Config
myconf conf =
  conf
   { benchlist = all_benchmarks,
     plugIns = [SomePlugin defaultDribblePlugin],
     harvesters = customHarvester `mappend` (harvesters conf)
   }


