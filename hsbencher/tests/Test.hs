
-- | Unit testing for functions in the HSBencher implementation.

module Main where

import           Data.List
import           Data.Maybe
import           Data.Default
import           HSBencher.Types
import           HSBencher.Internal.Config (getConfig)
import           HSBencher.Internal.MeasureProcess ()
import           HSBencher.Harvesters (fromTaggedLine)
import qualified Data.ByteString.Char8 as B

import           Test.Framework.Providers.HUnit
import           Test.Framework (Test, defaultMain, testGroup)
-- import Test.Framework.TH (testGroupGenerator) -- [2014.05.23] Disabling because of haskell-src-exts dependency and its very slow compiles.
import           Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))

-- import           Test.QuickCheck ()

--------------------------------------------------------------------------------

exampleOuptut :: [String]
exampleOuptut =
 [ "SELFTIMED: 3.3",
   "  14,956,751,416 bytes allocated in the heap",
   "       2,576,264 bytes copied during GC",
   "       5,372,024 bytes maximum residency (6 sample(s))",
   "       4,199,552 bytes maximum slop",
   "              14 MB total memory in use (0 MB lost due to fragmentation)",
   "",
   "                                    Tot time (elapsed)  Avg pause  Max pause",
   "  Gen  0     14734 colls, 14734 par    2.86s    0.15s     0.0000s    0.0006s",
   "  Gen  1         6 colls,     5 par    0.00s    0.00s     0.0002s    0.0005s",
   "",
   "  Parallel GC work balance: 37.15% (serial 0%, perfect 100%)",
   "",
   "  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N2)",
   "",
   "  SPARKS: 511 (1 converted, 0 overflowed, 0 dud, 0 GC'd, 510 fizzled)",
   "",
   "  INIT    time    0.00s  (  0.00s elapsed)",
   "  MUT     time    8.06s  (  5.38s elapsed)",
   "  GC      time    2.86s  (  0.15s elapsed)",
   "  EXIT    time    0.00s  (  0.00s elapsed)",
   "  Total   time   10.92s  (  5.53s elapsed)",
   "",
   "  Alloc rate    1,855,954,977 bytes per MUT second",
   "",
   "  Productivity  73.8% of total user, 145.7% of total elapsed",
   "",
   "gc_alloc_block_sync: 10652",
   "whitehole_spin: 0",
   "gen[0].sync: 8",
   "gen[1].sync: 1700" ]

-- case_prod = assertEqual "Harvest prod" [73.8] $
--               catMaybes $ map (fn . B.pack) exampleOuptut

case_harvest :: IO ()
case_harvest = do
  config <- getConfig [] []
  let LineHarvester fn = harvesters config
--  mapM_ print $ map (\x -> (x, fn (B.pack x))) exampleOuptut
  let hits = filter ((==True) . snd) $ map (fn . B.pack) exampleOuptut
  putStrLn$ "Lines harvested: "++show (length hits)
  let result = foldl' (\ r (f,_) -> f r) (def :: BenchmarkResult) hits
  let expected = def { _MEDIANTIME = 3.3
                     , _ALLTIMES = "3.3"
                     , _MEDIANTIME_PRODUCTIVITY = Just 73.8
                     , _MEDIANTIME_ALLOCRATE = Just 1855954977
                     , _MEDIANTIME_MEMFOOTPRINT = Just 5372024
                     , _ALLJITTIMES = []
                     , _CUSTOM = [] }

  assertEqual "Test harvesters" expected result

tests :: Test
tests = -- $(testGroupGenerator)
  testGroup "HSBencher-unit-tests"
  [ testCase "harvest" case_harvest
  ]

main :: IO ()
main = defaultMain [tests]
