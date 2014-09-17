{-# LANGUAGE DeriveDataTypeable #-} 

module Main where

import System.Environment (getArgs, getEnv, getEnvironment)
import System.Console.GetOpt (getOpt', ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.IO (Handle, hPutStrLn, stderr,stdin, openFile, hClose, hGetContents,
                  hIsEOF, hGetLine, IOMode(..), BufferMode(..), hSetBuffering)

import GHC.IO.Exception (IOException(..))

-- import qualified System.IO.Streams as Strm
-- import qualified System.IO.Streams.Concurrent as Strm
-- import qualified System.IO.Streams.Process as Strm
-- import qualified System.IO.Streams.Combinators as Strm

import Data.List (isInfixOf, intersperse, delete, transpose, sort,nub, deleteBy)
import Data.List.Split (splitOn)
import Data.String.Utils (strip)

import Control.Monad (unless,when)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn)

import Data.Char (isNumber)

-- Exceptions
import Control.Exception
import Data.Typeable

-- Charting library
import Graphics.Rendering.Chart as C
import Graphics.Rendering.Chart as C 
import Graphics.Rendering.Chart.Backend.Cairo as C
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens




import qualified Prelude as P
import Prelude hiding (init) 
---------------------------------------------------------------------------
--

{- DEVLOG
-} 




---------------------------------------------------------------------------
-- //                                                                 \\ --
---------------------------------------------------------------------------

-- | Command line flags to the benchmarking executable.
data Flag = ShowHelp | ShowVersion
          | File String
          | DataIn String  -- Readable as "Orientation"
          | OutFile String
          | RenderMode GraphMode
          | Title String
          | XLabel String
          | YLabel String
  -- BarCluster rendering related
          | GroupBy String -- Readable as LocationSpec

  -- Create a key from some number of columns
  -- This becomes the "name"
          | Key String --Readable as [Int]
            
            
  deriving (Eq,Ord,Show,Read)

data Orientation = Rows | Columns
                 deriving (Eq, Ord, Show, Read )

data LocationSpec = Row Int | Column Int 
                  deriving (Eq, Ord, Show, Read )
                           
data GraphMode = Bars | BarClusters | Lines 
               deriving (Eq, Ord, Show, Read )

-- | Type of values stored in a series
data ValueType = Int | Double | String  -- Maybe just Double | String ! 
               deriving (Eq, Ord, Show, Read )

-- | Exceptions that may occur
data Error
  = FlagsNotValidE String
    deriving (Show, Typeable) 

instance Exception Error 

-- | Command line options.
core_cli_options :: [OptDescr Flag]
core_cli_options = 
     [ Option ['h'] ["help"] (NoArg ShowHelp)
        "Show this help message and exit."
     , Option ['f'] ["file"] (ReqArg File "FileName.csv")    "Use a CSV file as input"
     , Option ['d'] ["data"] (ReqArg DataIn "Rows/Columns")  "Data series are along a row/column"
     , Option ['o'] ["out"]  (ReqArg OutFile "FileName.png") "Chart result file"
     , Option []    ["bars"] (NoArg (RenderMode Bars))       "Plot data as bars"
     , Option []    ["barclusters"] (NoArg (RenderMode BarClusters)) "Plot data as bar clusters" 
     , Option []    ["lines"] (NoArg (RenderMode Lines))     "Plot data as lines"
     , Option []    ["title"] (ReqArg Title "String")        "Plot title" 
     , Option []    ["xlabel"] (ReqArg XLabel "String")      "x-axis label"
     , Option []    ["ylabel"] (ReqArg YLabel "String")      "y-axis label"
     , Option []    ["key"]    (ReqArg Key "String")         "columns that make out the key [0,1,2]"
     , Option []    ["group"]  (ReqArg GroupBy "String")     "column to use as group identifier" 
     ]

-- | Multiple lines of usage info help docs.
fullUsageInfo :: String
fullUsageInfo = usageInfo docs core_cli_options
 where 
  docs = "USAGE: grapher <flags> ...\n"++
         "\n\nhsbencher-graph general options: \n"
--   ++ generalUsageStr

---------------------------------------------------------------------------
-- MAIN                                                                  --
---------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs

  let (options,plainargs,_unrec,errs) = getOpt' Permute core_cli_options args

  let outputSpecified = (not . null) [() | OutFile _ <- options]
      outfile = head [nom | OutFile nom <- options] 

      
  unless (null errs) $ do
    putStrLn$ "Errors parsing command line options:"
    mapM_ (putStr . ("   "++)) errs       
    exitFailure

  when (ShowHelp `elem` options) $ do 
    putStrLn fullUsageInfo
    exitSuccess

  when (not outputSpecified) $ do
    putStrLn$ "Error: an output file has to be specified"
    exitFailure

  ---------------------------------------------------------------------------
  -- read csv from stdin. 

  csv <- getCSV 

  -- hPutStrLn stderr $ show csv

  -- let types = map recogValueType (map tail csv)

  -- hPutStrLn stderr $ show types


  ---------------------------------------------------------------------------
  -- Perform the task specified by the command line args

  -- apply key
  -- These transformations should check is a specified transformation
  -- is sane given the plot style.
  -- List the rules for this. For example, a bar graph is not sane with more than one datapoint per "name"
  let csv_rekeyed = applyKey options csv
      csv_grouped = applyGroup options csv_rekeyed 
      
  putStrLn $ show csv_grouped 
  
  renderableToFile def (toRenderable (renderPlot options (take 10 csv_grouped))) outfile 
  return ()

    

---------------------------------------------------------------------------
-- Render a plot based on the options and the csv

--renderPlot :: [Flag] -> [[String]] -> FilePath -> IO ()
renderPlot flags csv = 
   case plotKind of
     [] -> error "No graph mode specified"
     (Bars:_) -> doBars series title xLabel yLabel
     (BarClusters:_) -> doBarClusters series cates title xLabel yLabel 
     (Lines:_) -> doLines series title xLabel yLabel 
     
                       
  where
    plotKind = [k | RenderMode k <- flags] 
    ---------------------------------------------------------------------------
    -- Set up defaults if none exist
    xLabel =
      case [lab | XLabel lab <- flags] of
        [] -> "x-axis"
        (l:_) -> l
    yLabel =
      case [lab | YLabel lab <- flags] of
        [] -> "y-axis"
        (l:_) -> l
    title = 
      case [x | Title x <- flags] of
        [] -> "Table title"
        (t:_) -> t

    series = mkSeries flags csv
    cates = barClusterCategories csv 
------------------------------------------------------------------------------
-- data serie  (all this need some work) 
data Serie = Serie {serieName :: String,
                    serieData :: [Double]  }


-- First row is supposed to be "informative", not data 
mkSeries :: [Flag] -> [[String]] -> [Serie]
mkSeries flags csv =
  case dataIn of
    Rows -> map rowsToSeries (tail csv)
    Columns -> map rowsToSeries $ transpose (tail csv)
  
  where
    dataIn =
      case [read x :: Orientation | DataIn x <- flags] of
        [] -> Rows
        (x:_) -> x

rowsToSeries :: [String] -> Serie 
rowsToSeries (name:rest) = Serie name (map read rest) 

-- make more solid !
barClusterCategories :: [[String]] -> [String]
barClusterCategories input =
  case  (all isInt cates || all isDouble cates) of
    -- Hey, these dont look like category names! it looks like data
    True -> cates -- ["category" ++ show n | n <- [0..length cates]]
    False -> cates
  where cates = tail $ head input 

-- Just a test.. (Now why is there a transpose in there !!!)
-- The transpose is related to not near groupBy. 
seriesToBarClusters :: [Serie] -> [[Double]]
seriesToBarClusters ss = transpose the_data 
  where
    the_data = map (\(Serie _ d) -> d) ss

-- Assumes one data point 
seriesToBars :: [Serie] -> [[Double]]
seriesToBars ss = map (\(Serie _ d) -> d) ss 

------------------------------------------------------------------------------
-- Plot bars
-- doBarClusters :: [Serie] -> [String] -> String -> String -> String -> IO ()
-- doBarClusters :: [Serie] -> [String] -> String -> String -> String -> Layout PlotIndex Double
doBarClusters series categories title xlabel ylabel = layout
 where
  layout = 
        layout_title .~ title
      $ layout_title_style . font_size .~ 10
      $ layout_x_axis . laxis_generate .~ autoIndexAxis alabels
      $ layout_y_axis . laxis_override .~ axisGridHide
      $ layout_left_axis_visibility . axis_show_ticks .~ False
      $ layout_plots .~ [ plotBars bars2 ]
      $ def :: Layout PlotIndex Double

  bars2 = plot_bars_titles .~ map serieName series -- ["Cash","Equity"]
      $ plot_bars_values .~ addIndexes (seriesToBarClusters series) -- [[20,45],[45,30],[30,20],[70,25]]
      $ plot_bars_style .~ BarsClustered
      $ plot_bars_spacing .~ BarsFixGap 30 5
      $ plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq)
      $ def

  alabels = categories -- [ "test" ]

  bstyle = Just (solidLine 1.0 $ opaque black)
  mkstyle c = (solidFillStyle c, bstyle)


--doBars :: [Serie] -> String -> String -> String -> SOMETHING!!!
doBars series title xlabel ylabel = layout
  where
    layout =
        layout_title .~ title
      $ layout_title_style . font_size .~ 10
      $ layout_x_axis . laxis_generate .~ autoIndexAxis (map serieName series)
      $ layout_y_axis . laxis_override .~ axisGridHide
      $ layout_left_axis_visibility . axis_show_ticks .~ False
      $ layout_plots .~ [ plotBars bars ]
      $ def :: Layout PlotIndex Double

    
    bars =
        plot_bars_titles .~ []
      $ plot_bars_values .~ addIndexes (seriesToBars series) -- [[20,45],[45,30],[30,20],[70,25]]
      $ plot_bars_style .~ C.BarsClustered
      $ plot_bars_spacing .~ BarsFixGap 30 5
      $ plot_bars_item_styles .~ map mkstyle (repeat (head (cycle defaultColorSeq)))
      $ def

    bstyle = Just (solidLine 1.0 $ opaque black)
    mkstyle c = (solidFillStyle c, bstyle)


------------------------------------------------------------------------------
-- Plot lines
-- doLines :: [Serie] -> String -> String -> String -> IO ()
doLines = undefined

---------------------------------------------------------------------------
-- Get the CSV from stdin

getCSV :: IO [[String]]
getCSV = do
  res <- catch (do
                   s <- hGetLine stdin
                   return $ Just s)
               (\(IOError _ _ _ _ _ _ )-> return Nothing)
  case res of
    Just str -> do rest <- getCSV
                   let str' = map strip $ splitOn "," str
                   return $ str' : rest
    Nothing -> return [] 



---------------------------------------------------------------------------
-- Recognize data


-- The rules.
-- The string contains only numerals -> Int
-- The string contains only numerals and exactly one . -> Double
-- the string contains exactly one . and an e -> Double 
-- The string contains any non-number char -> String

-- there is an ordering to the rules.
-- # 1 If any element of the
--     input contains something that implies String. They are all interpreted as Strings
-- # 2 If not #1 and any element contains . or . and e All values are doubles
-- # 3 If not #1 and #2 treat as Int

-- May be useless. just treat all values as "Double" 

-- | Figure out what type of value is stored in this data series.     
recogValueType :: [String] -> ValueType
recogValueType strs =
  case (any isString strs, any isInt strs, any isDouble strs) of
    (True,_,_)     -> String
    (False,True, False)  -> Int
    (False,_, True)  -> Double
    (_, _, _) -> String
  

isInt str = all isNumber str
    -- Not a very proper check. 
isDouble str = ((all isNumber $ delete '.' str)
                || (all isNumber $ delete '.' $ delete 'e' str)
                || (all isNumber $ delete '.' $ delete 'e' $ delete '-' str))
               && '.' `elem` str
isString str = not (isInt str) && not (isDouble str)


---------------------------------------------------------------------------
-- Try Sorting as numbers

trySortAsNum :: [String] -> Maybe [String]
trySortAsNum str =
  case valueType of
    -- If they are Integers I dont see how this can fail
    Int ->  Just $ map show $ sort $ (map read str :: [Int])
    -- If they are doubles, mae sure read/show invariant holds
    Double ->
      let sorted = map show $ sort $ (map read str :: [Double])
      in if (all (\x -> elem x sorted) str) then Just sorted else Nothing

    -- If they are words. Well, could sort, but doing nothing. 
    String -> Nothing 
      
  where 
    valueType = recogValueType str 





---------------------------------------------------------------------------
-- apply the key. that is move some columns to the "front" and concatenate
-- their contents

applyKey :: [Flag] -> [[String]] -> [[String]]
applyKey flags csv =
  if keyActive
  then map doIt csv
       
       
  else csv 
  where
    keyActive = (not . null) [() | Key _ <- flags]
    key = head [read x :: [Int] |  Key x <- flags]

    key_sorted_reverse = reverse $ sort key 

    fix_row_head r = concatMap (\i -> r !! i) key
    fix_row_tail r = dropCols key_sorted_reverse r

    doIt r = fix_row_head r : fix_row_tail r 

    dropCol x r = take x r ++ drop (x+1) r
    dropCols [] r = r 
    dropCols (x:xs) r = dropCols xs (dropCol x r)
    
       
        
-- Probably not very flexible. More thinking needed 
applyGroup :: [Flag] -> [[String]] -> [[String]]
applyGroup flags csv = 
  if groupActive
  then
    --error $ "\n\n" ++ show csv ++ "\n\n" ++ show theGroups ++
    --        "\n\n" ++ show theNames ++ "\n\n" ++ show allGroups ++
    --        "\n\n" ++ show doIt 
            
    case groupingIsValid of
      True -> doIt -- groupBy csv
      False -> error "Current limitation is that a table needs exactly three fields to apply grouping"
  else csv

  where
    groupActive = (not . null) [() | GroupBy _ <- flags]
    groupBy = head [read x :: Int |  GroupBy x <- flags]

    getKey r = head r

    groupingIsValid = all (\r -> length r == 3) csv

    theGroups' = nub $ sort $  map (\r -> r !! groupBy) csv 
    theGroups  =
      case trySortAsNum theGroups' of
        Nothing -> theGroups
        Just sorted -> sorted 

    --- Ooh dangerous!!! 
    valueIx = head $ delete groupBy [1,2]
    
    -- makes unreasonable assumptions! (ordering) 
    -- some sorting needs to be built in for the general case. 
    theNames = nub $ map head csv 

    --extractValues g rows = [r !! valueIx | r <- rows
     --                                    , g == (r !! groupBy)]

    allGroups = map (\n -> extractValues n csv) theNames --theGroups


    -- for each NAME. Pull out all values 
    extractValues name rows = organize theGroups $ [(r !! groupBy, r !! valueIx) | r <- rows ,getKey r == name] 

    --organize the values in the order specified by "theGroups"
    organize [] [] = []
    organize [] xs  = error $ show xs
    organize (x:xs) ys = 
      case lookup x ys of
        Nothing -> error "applyGroup: Bug alert!"
        Just y  -> y: (organize xs (deleteBy (\(a,_) (b,_) -> a == b) (x,"") ys) )
    
    
    doIt = ("KEY" : theGroups) : zipWith (\a b -> a : b) theNames allGroups 

-- This is what you get. 
-- groupBy "THREADS" 
-- Key     1   2   4   8   16
-- key1    v1  v2  v3  v4  v5

-- But it seems having data series in columns is the standard approach.
-- So this would be desireable.
-- groupBy "THREADS"

--  Threads key1 key2 key3 key4 
--  1       v1   w1   u1   ü1    
--  2       v2   w2   u2   ü2
--  4       v3   w3   u3   ü3

-- New applyGroup that uses a different result layout.
-- I think this is the layout more often requested by plotting tools. 
applyGroup :: [Flag] -> [[String]] -> [[String]]
applyGroup flags csv = 
  if groupActive
  then
            
    case groupingIsValid of
      True -> doIt -- groupBy csv
      False -> error "Current limitation is that a table needs exactly three fields to apply grouping"
  else csv

  where
    groupActive = (not . null) [() | GroupBy _ <- flags]
    -- This could also potentially be a name, right ? 
    groupSpecifierColumn = head [read x :: Int |  GroupBy x <- flags]

    -- After rearrangement the key will be at the head of the list
    -- (Thats why grouping requires keying) 
    getKey r = head r

    -- I have no idea what to do if the table has too many column,
    -- Could require that the user specify Key, Group and Value column,
    -- and filter out those in the process of grouping. 
    groupingIsValid = all (\r -> length r == 3) csv

    -- Read out all different values in the groupSpecifierColumn.
    theGroups' = nub $ sort $  map (\r -> r !! groupSpecifierColumn) csv 
    theGroups  =
      case trySortAsNum theGroups' of
        Nothing -> theGroups
        Just sorted -> sorted 

    --- Ooh dangerous!!!
    --  But should work if key is in column 0 (user has rekeyed) 
    --  and if number of columns is 3. 
    valueIx = head $ delete groupSpecifyerColumn [1,2]
    

    -- extract all the different keys.
    -- Could try to sort these as numbers as well.. 
    allKeys = nub $ sort $ map head csv 

    allGroups = map (\n -> extractValues n csv) theNames --theGroups


    -- for each NAME. Pull out all values 
    extractValues name rows = organize theGroups $ [(r !! groupBy, r !! valueIx) | r <- rows ,getKey r == name] 

    --organize the values in the order specified by "theGroups"
    organize [] [] = []
    organize [] xs  = error $ show xs
    organize (x:xs) ys = 
      case lookup x ys of
        Nothing -> error "applyGroup: Bug alert!"
        Just y  -> y: (organize xs (deleteBy (\(a,_) (b,_) -> a == b) (x,"") ys) )
    
    
    doIt = ("KEY" : theGroups) : zipWith (\a b -> a : b) theNames allGroups 


    -- Can potentially find a whole list of values at a given key
    -- and a given groupID
    extractGroup :: (String,Int) -> (String,Int) -> [[String]] -> (String,(String, [String]))
    extractGroup (key,keyix) (groupElem,gix) table = undefined 
