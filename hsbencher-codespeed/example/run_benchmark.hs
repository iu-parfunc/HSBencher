#!/usr/bin/env runghc
{-# LANGUAGE NamedFieldPuns #-}

import Data.Monoid 
import HSBencher
import HSBencher.Backend.Codespeed
import Debug.Trace
import Data.Default(Default(def))

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
defaultCfgSpc :: BenchSpace meaning
defaultCfgSpc = And []

-- | Here we have the option of changing the HSBencher config
myconf :: Config -> Config
myconf conf0 = 
  trace ("Modifying config to "++show conf2) conf2
 where
  thePlug = defaultCodespeedPlugin
  csconf = def { codespeedURL = "http://codespeed.crest.iu.edu"
               , projName     = "ExampleProject" }
  conf2 = setMyConf thePlug csconf conf1
  conf1 = (conf0
   { benchlist = all_benchmarks,
     plugIns = [SomePlugin thePlug],
     -- harvesters = customHarvester1 `mappend` customHarvester `mappend` (harvesters conf)
     harvesters = customTagHarvesterInt "MYTAG" `mappend`
                  customTagHarvesterDouble "ANOTHERTAG" `mappend`
                  harvesters conf0
   })
