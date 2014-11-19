{-# LANGUAGE NamedFieldPuns #-}

-- | Code for dealing with the `BenchSpace` datatype.

module HSBencher.Internal.BenchSpace
       (BenchSpace(..), enumerateBenchSpace,
        filterBenchmarks, disjunctiveNF)
       where

import Data.Maybe
import qualified Data.Set as S
import Data.List
import HSBencher.Types

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
-- somewhere inside them.
-- 
-- A note on semantics:
--   A benchmark (+configs) is treated really a STREAM of concrete benchmark instances.
-- 
--   Each pattern filters out a subset of these instances (either by
--   matching a varying or a static field).  This function returns a
--   benchmark that iterates over the INTERSECTION of those subsets.
-- 
filterBenchmarks :: [String]  -- ^ Patterns which must all be matched.
                 -> [Benchmark DefaultParamMeaning] -> [Benchmark DefaultParamMeaning]
filterBenchmarks [] = id -- Don't convert to disjunctive normal form!
filterBenchmarks patterns = mapMaybe fn
  where
   fn orig@Benchmark{target,cmdargs,progname,configs} = 
     let unmet = [ pat | pat <- patterns
                       , not (isInfixOf pat target ||
                              isInfixOf pat (fromMaybe "" progname) ||
                              any (isInfixOf pat) cmdargs) ]
     in case filtConfigs unmet configs of
          Or [] -> Nothing
          newcfgs -> Just (orig { configs = newcfgs } )
               
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
  g [] Set{} = []
  g (hd:pats) (Set ls1 ls2) =
    if isInfixOf hd (show ls1) || isInfixOf hd (show ls2)
    then      g pats (Set ls1 ls2)
    else hd : g pats (Set ls1 ls2)

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

addAnd :: BenchSpace meaning -> BenchSpace meaning -> BenchSpace meaning
addAnd (Or []) _  = Or []
addAnd x (And ls) = And (x:ls)
addAnd x y        = And [x,y]

addOr :: BenchSpace t -> BenchSpace t -> BenchSpace t
addOr (Or []) rst = rst
addOr x (Or ls)   = Or (x:ls)
addOr x rst       = Or [x,rst]

mkOr :: [BenchSpace meaning] -> BenchSpace meaning
mkOr [] = Or []
mkOr (x : tl) = addOr x (mkOr tl)

intersections :: Ord a => [S.Set a] -> S.Set a
intersections [] = error "No set intersection of the empty list"
intersections [s] = s
intersections (s1:sets) = S.intersection s1 (intersections sets)

bp1 :: BenchSpace DefaultParamMeaning
bp1 = (And [Set (Variant "Reduce") (RuntimeArg "Reduce"),
            Set NoMeaning (RuntimeArg "r6") ])

t1 = filtConfigs ["Reduce", "r6"] bp1

t2 = filtConfigs ["Reduce", "r6"] (Or [ bp1, Set NoMeaning (RuntimeEnv "FOO" "r6") ])

