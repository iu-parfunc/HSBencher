import System.Directory
import System.FilePath
import System.Exit
import System.Environment (getArgs)

import HSBencher.Analytics


import Data.List hiding (lines)
import Data.Maybe 
import Prelude hiding (log, lines)

-- Temporarily
import Network.Google.FusionTables 

--clientid=762414730219-86ib5a8puqj2n0cmo6mcvvbimdrpo6o0.apps.googleusercontent.com
--clientsecret=-C7224t9hdv_scoKJ5p4fN8l

cid = "762414730219-86ib5a8puqj2n0cmo6mcvvbimdrpo6o0.apps.googleusercontent.com"
csec = "-C7224t9hdv_scoKJ5p4fN8l" 

-- 4/NpKXnmRt0VfkudsNl2YaA1v7qtQG.IiO0oB-bMbcVeFc0ZRONyF5ddwlvjgI

main :: IO ()
main = do
  -- tab <- pullEntireTable cid csec "ExampleTable" 
  tab <- pullSelectively cid csec "ExampleTable" "GIT_DEPTH" "408"
  
  putStrLn $ show tab

  let (ColData cols values) = tab

  putStrLn "--------------------------------------------------"
  putStrLn $ show cols

  putStrLn "--------------------------------------------------"
  putStrLn $ show (head values) 
  

  let cuda_values = slice "RUNID" "N56VZ_1401906062" cols 
                    $ slice "NAME" "CUDA" cols values 
  
  let cuda_threads = map convertInt $ extractColumn "NUMTHREADS" cols cuda_values 
      cuda_times   = map convert $ extractColumn "MEDIANTIME" cols cuda_values 
      cuda_threadsTime = zip cuda_threads cuda_times 

  let barGraph = BarGraph "#F00" 
                          "CUDA" 
                          (map (\(x,y) -> (show x,y))
                           (sortBy (\a b -> compare (fst a) (fst b)) 
                            cuda_threadsTime))

  let plot = Plot {pLines = [],
                   pPoints = [], 
                   pBars   = [barGraph,barGraph],
                   pLegend = True,
                   pDimensions = (800,400), 
                   pXLabel = "Threads", 
                   pYLabel = "ms"} 

                           
      -- my_lines = [("cuda", sortBy (\a b -> compare (fst a) (fst b)) cuda_threadsTime)]
  

      
  putStrLn "Plotting example plot..."


  putStrLn $ html $ renderPlot mySupply plot

  --plotLines "example1.png" [("version1", [(0,1),(1,2),(2,3),(4,5)]),
  --                           ("version2", [(0,6),(1,5),(2,7),(4,8)])]
  --plotLines my_lines "CUDA-Experiment" "example1.png"
  --plotB ["128","256","512","1024"]
  --      (map snd (sortBy (\a b -> compare (fst a) (fst b)) cuda_threadsTime))
  --      "CUDA-Experiment" "example2.png" 
  
  return () 


-- convert is cheating a bit. 
convert :: FTValue -> Double
convert (DoubleValue v) = v
convert (StringValue s) = read s 

convertInt :: FTValue -> Int
convertInt (DoubleValue v) = truncate v
convertInt (StringValue s) = truncate $ (read s :: Double)

extractColumn str header tab =
  [x !! ix | x <- tab] 
  where
    ix  = fromJust $ elemIndex str header 
   


slice col value header values =
  [x | x <- values , x !! ix == (StringValue value)] 
  where
    ix = fromJust $ elemIndex col header 
