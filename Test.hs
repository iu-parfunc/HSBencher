
import Data.List
import Data.Maybe
import HSBencher.MeasureProcess
import HSBencher.Types
import qualified Data.ByteString.Char8 as B

exampleOuptut :: [String]
exampleOuptut = 
 [ "  14,956,751,416 bytes allocated in the heap",
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

main :: IO ()
main = do
  let LineHarvester fn = ghcProductivityHarvester
--  mapM_ print $ map (\x -> (x, fn (B.pack x))) exampleOuptut
  let hits = filter ((==True) . snd) $ map (fn . B.pack) exampleOuptut
  putStrLn$ "Lines matched: "++show (length hits)
  print $ foldl' (\ r (f,_) -> f r) emptyRunResult hits
  
  return ()

