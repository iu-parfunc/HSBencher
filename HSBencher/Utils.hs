{-# LANGUAGE NamedFieldPuns #-}

-- | Misc Small Helpers

module HSBencher.Utils where

import qualified Data.Set as Set
import Data.Char (isSpace)
import Data.List (isPrefixOf)

import HSBencher.Types

--------------------------------------------------------------------------------

-- These int list arguments are provided in a space-separated form:
parseIntList :: String -> [Int]
parseIntList = map read . words 

-- Remove whitespace from both ends of a string:
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- | Parse a simple "benchlist.txt" file.
parseBenchList :: String -> [Benchmark]
parseBenchList str = 
  map parseBench $                 -- separate operator, operands
  filter (not . null) $            -- discard empty lines
  map words $ 
  filter (not . isPrefixOf "#") $  -- filter comments
  map trim $
  lines str

-- Parse one line of a benchmark file (a single benchmark name with args).
parseBench :: [String] -> Benchmark
parseBench (h:m:tl) = Benchmark {name=h, compatScheds=expandMode m, args=tl }
parseBench ls = error$ "entry in benchlist does not have enough fields (name mode args): "++ unwords ls

strBool :: String -> Bool
strBool ""  = False
strBool "0" = False
strBool "1" = True
strBool  x  = error$ "Invalid boolean setting for environment variable: "++x

fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c

-- Compute a cut-down version of a benchmark's args list that will do
-- a short (quick) run.  The way this works is that benchmarks are
-- expected to run and do something quick if they are invoked with no
-- arguments.  (A proper benchmarking run, therefore, requires larger
-- numeric arguments be supplied.)
-- 
-- HOWEVER: there's a further hack here which is that leading
-- non-numeric arguments are considered qualitative (e.g. "monad" vs
-- "sparks") rather than quantitative and are not pruned by this
-- function.
shortArgs :: [String] -> [String]
shortArgs [] = []
-- Crop as soon as we see something that is a number:
shortArgs (h:tl) | isNumber h = []
		 | otherwise  = h : shortArgs tl

isNumber :: String -> Bool
isNumber s =
  case reads s :: [(Double, String)] of 
    [(n,"")] -> True
    _        -> False

-- Based on a benchmark configuration, come up with a unique suffix to
-- distinguish the executable.
uniqueSuffix :: BenchRun -> String
uniqueSuffix BenchRun{threads,sched,bench} =    
  "_" ++ show sched ++ 
   if threads == 0 then "_serial"
                   else "_threaded"


-- Indent for prettier output
indent :: [String] -> [String]
indent = map ("    "++)


--------------------------------------------------------------------------------
-- TODO -- all this Mode selection should be factored out to make this
-- benchmark script a bit more generic.
--------------------------------------------------------------------------------


-- [2012.05.03] RRN: ContFree is not exposed, thus removing it from the
-- default set, though you can still ask for it explicitly:
defaultSchedSet :: Set.Set Sched
defaultSchedSet = Set.difference (Set.fromList [minBound ..])
                               (Set.fromList [ContFree, NUMA, SMP])

-- Omitting ContFree, as it takes way too long for most trials
ivarScheds :: [Sched]
ivarScheds = [Trace, Direct, SMP, NUMA] 
-- ivarScheds = [Trace, Direct]

-- TODO -- we really need to factor this out into a configuration file.
schedToModule :: Sched -> String
schedToModule s = 
  case s of 
--   Trace    -> "Control.Monad.Par"
   Trace    -> "Control.Monad.Par.Scheds.Trace"
   Direct   -> "Control.Monad.Par.Scheds.Direct"
   ContFree -> "Control.Monad.Par.Scheds.ContFree"
   Sparks   -> "Control.Monad.Par.Scheds.Sparks"
   SMP      -> "Control.Monad.Par.Meta.SMP"
   NUMA     -> "Control.Monad.Par.Meta.NUMAOnly"
   None     -> "qualified Control.Monad.Par as NotUsed"

schedToCabalFlag :: Sched -> String
schedToCabalFlag s =
  case s of
    Trace -> "--flags=\"-ftrace\""
    Direct -> "--flags=\"-fdirect\""
    ContFree -> "--flags=\"-fcontfree\""
    Sparks -> "--flags=\"-fsparks\""
    SMP -> "--flags=\"-fmeta-smp\""
    NUMA -> "--flags=\"-fmeta-numa\""
    None -> ""

-- TODO - GET RID OF THIS:
-- | Expand the mode string into a list of specific schedulers to run:
expandMode :: String -> [Sched]
expandMode "default" = [Trace]
expandMode "none"    = [None]
-- TODO: Add RNG:
expandMode "futures" = [Sparks] ++ ivarScheds
expandMode "ivars"   = ivarScheds 
expandMode "chans"   = [] -- Not working yet!

-- [future] Schedulers in which nested execution WORKS!
expandMode "nested"      = [Sparks,Direct] -- [2012.11.26]
expandMode "nested+ivar" = [Direct]        -- [2012.11.26]

-- Also allowing the specification of a specific scheduler:
expandMode "Trace"    = [Trace]
expandMode "Sparks"   = [Sparks]
expandMode "Direct"   = [Direct]
expandMode "ContFree" = [ContFree]
expandMode "SMP"      = [SMP]
expandMode "NUMA"     = [NUMA]

expandMode s = error$ "Unknown Scheduler or mode: " ++s


--------------------------------------------------------------------------------

-- | In seconds.
defaultTimeout :: Double
defaultTimeout = 150
