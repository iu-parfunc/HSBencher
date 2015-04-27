{-# LANGUAGE NamedFieldPuns #-}

-- | Code for dealing with the `BenchSpace` datatype.

module HSBencher.Internal.BenchSpace
       (BenchSpace(..), enumerateBenchSpace,
        benchSpaceSize, 
        filterBenchmarks, filterBenchmark,
        disjunctiveNF)
       where

import Data.Maybe
import Data.List
import HSBencher.Types


-- | The size of a configuration space. This is equal to the length of
-- the result returned by `enumerateBenchSpace`, but is quicker to
-- compute.
benchSpaceSize :: BenchSpace a -> Int
benchSpaceSize Set{} = 1
benchSpaceSize (And x) = product $ map benchSpaceSize x 
benchSpaceSize (Or x)  = sum $ map benchSpaceSize x

-- | Exhaustively compute all configurations described by a benchmark configuration space.
enumerateBenchSpace :: BenchSpace a -> [ [(a,ParamSetting)] ] 
enumerateBenchSpace bs =
  case bs of
    Set m p -> [ [(m,p)] ]
    Or ls -> concatMap enumerateBenchSpace ls
    And ls -> loop ls
  where
    loop [] = [ [] ]  -- And [] => one config
    loop [lst] = enumerateBenchSpace lst
    loop (hd:tl) =
      let confs = enumerateBenchSpace hd in
      [ c++r | c <- confs
             , r <- loop tl ]


-- | Filter down a list of benchmarks (and their configuration spaces)
-- to only those that have ALL of the pattern arguments occurring
-- *somewhere* in their printed representation.
--
-- This completely removes any benchmark with an empty configuration
-- space (`Or []`).
filterBenchmarks :: [String]  -- ^ Patterns which must all be matched.
                 -> [Benchmark DefaultParamMeaning] -> [Benchmark DefaultParamMeaning]
filterBenchmarks [] = id -- Don't convert to disjunctive normal form!
filterBenchmarks patterns = mapMaybe fn
  where
   fn b = case filterBenchmark patterns b of
            Benchmark{configs} | configs == Or[] -> Nothing
                               | otherwise       -> Just b

-- | Filter down the config space of a benchmark, to only those
--  configurations that have a match for ALL of the pattern arguments
--  occurring somewhere inside them.
-- 
-- A note on semantics:
--
--   A benchmark (with its config space) implies a STREAM of concrete
--   benchmark instances.
-- 
--   Each pattern filters out a subset of these instances (either by
--   matching a varying field like `RuntimeEnv` or a static field like
--   `progname`).  This function conjoins all the patterns and thus
--   returns a benchmark that iterates over the INTERSECTION of those
--   subsets implied by each pattern, respectively.
-- 
filterBenchmark :: [String]  -- ^ Patterns which must all be matched.
                 -> Benchmark DefaultParamMeaning -> Benchmark DefaultParamMeaning
filterBenchmark patterns orig@Benchmark{target,cmdargs,progname,configs} = 
     let unmet = [ pat | pat <- patterns
                       , not (isInfixOf pat target ||
                              isInfixOf pat (fromMaybe "" progname) ||
                              any (isInfixOf pat) cmdargs) ]
         newcfgs = filtConfigs unmet configs
     in orig { configs = newcfgs }

               
-- Returns Or [] for an empty config space.
filtConfigs :: Show a => [String] -> BenchSpace a -> BenchSpace a
filtConfigs pats bs =
  let Or ls = disjunctiveNF bs
  in Or [ And as | And as <- ls, andMatch pats as ]

-- Does a conjoined sequence of Set clauses match ALL patterns?
andMatch :: Show a => [String] -> [BenchSpace a] -> Bool
andMatch pats0 ls = null (f pats0 ls)
 where 
  f []   _  = []
  f pats [] = pats
  f pats (x@Set{} : rst) = let pats' = g pats x
                           in f pats' rst
  f _ (And{} : _) = error "BenchSpace.hs/andMatch: internal invariant broken."
  f _ (Or{}  : _) = error "BenchSpace.hs/andMatch: internal invariant broken."

  g [] Set{} = []
  g (hd:pats) (Set ls1 ls2) =
    if isInfixOf hd (show ls1) || isInfixOf hd (show ls2)
    then      g pats (Set ls1 ls2)
    else hd : g pats (Set ls1 ls2)
  g _ (And{}) = error "BenchSpace.hs/andMatch: internal invariant broken."
  g _ (Or{} ) = error "BenchSpace.hs/andMatch: internal invariant broken."

-- | Convert to disjunctive normal form.  This can be an exponential
-- increase in the size of the value.
disjunctiveNF :: BenchSpace a -> BenchSpace a
disjunctiveNF = Or . map And . loop 
 where
 loop bs =
  case bs of
    Set _ _ -> [[bs]]
    And []  -> [[]]
    And (h:t) -> [ x++y | x <- loop h
                        , y <- loop (And t) ]
    Or ls -> concatMap loop ls

_bp1 :: BenchSpace DefaultParamMeaning
_bp1 = (And [Set (Variant "Reduce") (RuntimeArg "Reduce"),
            Set NoMeaning (RuntimeArg "r6") ])

_t1 :: BenchSpace DefaultParamMeaning
_t1 = filtConfigs ["Reduce", "r6"] _bp1

_t2 :: BenchSpace DefaultParamMeaning
_t2 = filtConfigs ["Reduce", "r6"] (Or [ _bp1, Set NoMeaning (RuntimeEnv "FOO" "r6") ])

