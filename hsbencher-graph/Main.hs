{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Environment (getArgs, getEnv, getEnvironment)
import System.Console.GetOpt (getOpt', ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.IO (Handle, hPutStrLn, stderr,stdin, openFile, hClose, hGetContents,
                  hIsEOF, hGetLine, IOMode(..), BufferMode(..), hSetBuffering)

import GHC.IO.Exception (IOException(..))

import Data.List (isInfixOf, intersperse, delete, transpose, sort,nub, deleteBy)
import Data.List.Split (splitOn)
import Data.String.Utils (strip)

import Control.Monad (unless,when)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn)

import Data.Char (isNumber)

import qualified Data.Map as M 

-- list tweaking
import Data.List (elemIndex, intersperse, sortBy)


import Data.Maybe (catMaybes)

-- Exceptions
import Control.Exception
import Data.Typeable

-- Charting library
import Graphics.Rendering.Chart as C
import Graphics.Rendering.Chart as C 
import Graphics.Rendering.Chart.Backend.Cairo as C
import Graphics.Rendering.Chart.Easy as C
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens




import qualified Prelude as P
import Prelude hiding (init) 
---------------------------------------------------------------------------
--


{- DEVLOG

  - 20 Nov 2014: Starting some simplifications and cleanups


-} 



---------------------------------------------------------------------------
-- 
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

-- identify part of a key 
          | Key String -- --key="Arg1" --key?"Arg2" means Arg1_Arg2 is the name of data series
          | XValues String -- column containing x-values
          | YValues String -- column containing y-values 
            
            
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
     , Option []    ["key"]    (ReqArg Key "String")         "columns that make part of the key"
     , Option []    ["group"]  (ReqArg GroupBy "String")     "column to use as group identifier"
     , Option ['x'] ["xvalue"] (ReqArg XValues "String")      "Column containing x values"
     , Option ['y'] ["yvalue"] (ReqArg YValues "String")      "Column containing y values"  
     ]

-- | Multiple lines of usage info help docs.
fullUsageInfo :: String
fullUsageInfo = usageInfo docs core_cli_options
 where 
  docs = "USAGE: grapher <flags> ...\n"++
         "\n\nhsbencher-graph general options: \n"
--   ++ generalUsageStr

---------------------------------------------------------------------------
-- All series in a map.
---------------------------------------------------------------------------
data SeriesData = IntData Int 
                | NumData Double
                | StringData String
                  deriving Show 

isString :: SeriesData -> Bool
isString (StringData _) = True
isString _ = False

isNum :: SeriesData -> Bool
isNum (NumData _) = True
isNum _ = False

isInt :: SeriesData -> Bool
isInt (IntData _) = True
isInt _ = False 

seriesType :: SeriesData -> ValueType
seriesType (IntData _) = Int
seriesType (NumData _) = Double
seriesType (StringData _) = String 

class FromData a where
  fromData :: SeriesData -> a

instance FromData Double where
  fromData (NumData a) = a
  fromData _ = error "FromData Double"

instance FromData Int where
  fromData (IntData a) = a
  fromData _ = error "FromData Int"

instance FromData String where
  fromData (StringData a) = a
  fromData _ = error "FromData String" 

type DataSeries = M.Map String [(SeriesData,SeriesData)] 

insertVal :: DataSeries -> String -> (SeriesData,SeriesData) -> DataSeries
insertVal m key val@(x,y) =
  case M.lookup key m of
    Nothing -> M.insert key [val] m 
    Just vals -> M.insert key (val:vals) m

    


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
  -- Get keys
  let hasKey  = (not . null) [ () | Key _ <- options]  
      key =
        case hasKey of
          False -> error "No key specified" 
          True  -> [ q | Key q <- options] 

  let hasX    = (not . null) [ () | XValues _ <- options]  
      hasY    = (not . null) [ () | YValues _ <- options]  

      xy =
        case hasX && hasY of
          False -> error "Both -x and -y arguments are needed"
          True  -> (head [x | XValues x <- options],
                    head [y | YValues y <- options])
      
  ---------------------------------------------------------------------------
  -- read csv from stdin. 

  csv <- getCSV key xy
  
         
  hPutStrLn stderr $ "printing csv" 
  hPutStrLn stderr $ show csv

  ---------------------------------------------------------------------------
  -- Create a lineplot 

  let series :: [(String,[(SeriesData,SeriesData)])]
      series = M.assocs csv

      series' = map unifyTypes series
      series_type = typecheck series' 
      
  case series_type of
    Just (Int,Int) -> plotIntInt series'
    Just (Int,Double) -> plotIntDouble series'
    Just (Double,Double) -> plotDoubleDouble series'
    Just (_,_) -> error $ "no support for plotting of this series type: " ++ show series_type
    Nothing -> error $ "Series failed to typecheck" 
  
---------------------------------------------------------------------------
-- Plotting
    
plotIntInt series' = undefined


plotIntDouble series =
  toFile def "test.png" $ do

    layout_title .= "testplot from grapher"
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= (opaque black)
    layout_left_axis_visibility . axis_show_ticks .= True
    layout_title_style . font_size .= 24

    mapM_ plotIt series 
    
    -- plot (myline "g" [[(1,8.3),(2,7.6),(3,6.4)]::[(Int,Double)]])
  where
    plotIt (name,xys) =
      plot (myline name [(sortBy (\(x,_) (x',_) -> x `compare` x')  $ zip xsi ysd)])
      where 
        (xs,ys)  =  unzip xys
        xsi = map fromData xs :: [Int]
        ysd = map fromData ys :: [Double]

    myline :: String -> [[(x,y)]]  -> EC l (PlotLines x y)
    myline title values = liftEC $ do
      color <- takeColor
      plot_lines_title .= title
      plot_lines_values .= values
      plot_lines_style . line_color .= color
      plot_lines_style . line_width .= 5 

plotDoubleDouble = undefined 


---------------------------------------------------------------------------
-- Types in the data 

unifyTypes :: (String,[(SeriesData,SeriesData)])
              -> (String,[(SeriesData,SeriesData)])
unifyTypes (name,series) =
  let (xs,ys) = unzip series
      xs' = unify xs
      ys' = unify ys
  in (name,(zip xs' ys'))
  where
    unify xs =
      case (any isString xs, any isInt xs, any isNum xs) of
        (True, _, _) -> map convertToString xs
        (False,_,True) -> map convertToNum xs
        (False,True,False) -> xs
    convertToString (IntData x) = StringData (show x)
    convertToString (NumData x) = StringData (show x)
    convertToString a = a
    convertToNum (IntData x) = NumData (fromIntegral x)
    convertToNum (NumData x) = NumData x
    convertToNum (StringData str) = error $ "Attempting to convert string " ++ str ++ " to Num" 

typecheck :: [(String,[(SeriesData,SeriesData)])] -> Maybe (ValueType, ValueType) 
typecheck dat =
  let series = concatMap snd dat
      (xs,ys) = unzip series
  in
   case length xs >= 1 && length ys >= 1 of 
     True ->
       let x = seriesType $ head xs
           y = seriesType $ head ys

           xb = all (==x) $ map seriesType xs
           yb = all (==y) $ map seriesType ys
       in Just (x,y)
     False -> Nothing 

---------------------------------------------------------------------------
-- Get the CSV from stdin

getCSV :: [String] -> (String,String) -> IO DataSeries --[[String]]
getCSV keys (xcol,ycol)= do
  -- Read of the headers 
  res <- catch
             (do
                 s <- hGetLine stdin
                 return $ Just s
             )
             (\(IOError _ _ _ _ _ _ ) -> return Nothing)
  case res of
    Nothing -> error "Error trying to get csv header line"
    Just str -> do 
      let csv = map strip $ splitOn "," str
          keyStr = concat keys 
          keyIxs = catMaybes $ zipWith elemIndex keys (replicate (length keys) csv)

          -- dangerous! 
          Just xcolIx = elemIndex xcol csv
          Just ycolIx = elemIndex ycol csv
          
  --    putStrLn $ "Specified key: " ++ keyStr    
  --    putStrLn $ "Key Indices:   " ++ show keyIxs
  --    putStrLn $ "Read the headers line: " ++ str
      
        
      case (length keyIxs) == (length keys) of
        False -> error "Key is not part of table"
        True -> do
          m <- loop keyIxs (xcolIx,ycolIx) M.empty
  --        putStrLn $ show m
          return m 
  where
    loop keyIxs xy m = do
      res <- catch
             (do
                 s <- hGetLine stdin
                 return $ Just s
             )
             (\(IOError _ _ _ _ _ _ )-> return Nothing)
      case res of
        -- In the odd event that an empty line occurs 
        Just []  -> loop keyIxs xy m 
        Just str -> do 
          let csv = map strip $ splitOn "," str
          
              key = collectKey keyIxs csv      

              val  = collectXY xy csv
              m' = insertVal m key val
   --       putStrLn $ show csv
          loop keyIxs xy m'
        Nothing -> return m
        
    collectKey ixs csv = concat $ intersperse "_" $ map (\i -> csv !! i) ixs
    
    collectXY (x,y) csv =  ( toSeriesData $ collectVal x csv
                           , toSeriesData $ collectVal y csv)
    collectVal ix csv = csv !! ix 

    toSeriesData x =
      case recogValueType x of
        Int -> IntData (read x) 
        Double -> NumData (read x) 
        String -> StringData x
        
  -- let types = map recogValueType (map tail csv)

  -- hPutStrLn stderr $ show types


  ---------------------------------------------------------------------------
  -- Perform the task specified by the command line args

  -- apply key
  -- These transformations should check is a specified transformation
  -- is sane given the plot style.
  -- List the rules for this. For example, a bar graph is not sane with more than one datapoint per "name"
 --  let csv_rekeyed = applyKey options csv
--       csv_grouped = applyGroup options csv_rekeyed 
      
--   putStrLn $ show csv_grouped 
  
--   renderableToFile def (toRenderable (renderPlot options (take 10 csv_grouped))) outfile 
--   return ()

    

-- ---------------------------------------------------------------------------
-- -- Render a plot based on the options and the csv

-- --renderPlot :: [Flag] -> [[String]] -> FilePath -> IO ()
-- renderPlot flags csv = 
--    case plotKind of
--      [] -> error "No graph mode specified"
--      (Bars:_) -> doBars series title xLabel yLabel
--      (BarClusters:_) -> doBarClusters series cates title xLabel yLabel 
--      (Lines:_) -> doLines series title xLabel yLabel 
     
                       
--   where
--     plotKind = [k | RenderMode k <- flags] 
--     ---------------------------------------------------------------------------
--     -- Set up defaults if none exist
--     xLabel =
--       case [lab | XLabel lab <- flags] of
--         [] -> "x-axis"
--         (l:_) -> l
--     yLabel =
--       case [lab | YLabel lab <- flags] of
--         [] -> "y-axis"
--         (l:_) -> l
--     title = 
--       case [x | Title x <- flags] of
--         [] -> "Table title"
--         (t:_) -> t

--     series = mkSeries flags csv
--     cates = barClusterCategories csv 
-- ------------------------------------------------------------------------------
-- -- data serie  (all this need some work) 
-- data Serie = Serie {serieName :: String,
--                     serieData :: [Double]  }


-- -- First row is supposed to be "informative", not data 
-- mkSeries :: [Flag] -> [[String]] -> [Serie]
-- mkSeries flags csv =
--   case dataIn of
--     Rows -> map rowsToSeries (tail csv)
--     Columns -> map rowsToSeries $ transpose (tail csv)
  
--   where
--     dataIn =
--       case [read x :: Orientation | DataIn x <- flags] of
--         [] -> Rows
--         (x:_) -> x

-- rowsToSeries :: [String] -> Serie 
-- rowsToSeries (name:rest) = Serie name (map read rest) 

-- -- make more solid !
-- barClusterCategories :: [[String]] -> [String]
-- barClusterCategories input =
--   case  (all isInt cates || all isDouble cates) of
--     -- Hey, these dont look like category names! it looks like data
--     True -> cates -- ["category" ++ show n | n <- [0..length cates]]
--     False -> cates
--   where cates = tail $ head input 

-- -- Just a test.. (Now why is there a transpose in there !!!)
-- -- The transpose is related to not near groupBy. 
-- seriesToBarClusters :: [Serie] -> [[Double]]
-- seriesToBarClusters ss = transpose the_data 
--   where
--     the_data = map (\(Serie _ d) -> d) ss

-- -- Assumes one data point 
-- seriesToBars :: [Serie] -> [[Double]]
-- seriesToBars ss = map (\(Serie _ d) -> d) ss 

-- ------------------------------------------------------------------------------
-- -- Plot bars
-- -- doBarClusters :: [Serie] -> [String] -> String -> String -> String -> IO ()
-- -- doBarClusters :: [Serie] -> [String] -> String -> String -> String -> Layout PlotIndex Double
-- doBarClusters series categories title xlabel ylabel = layout
--  where
--   layout = 
--         layout_title .~ title
--       $ layout_title_style . font_size .~ 10
--       $ layout_x_axis . laxis_generate .~ autoIndexAxis alabels
--       $ layout_y_axis . laxis_override .~ axisGridHide
--       $ layout_left_axis_visibility . axis_show_ticks .~ False
--       $ layout_plots .~ [ plotBars bars2 ]
--       $ def :: Layout PlotIndex Double

--   bars2 = plot_bars_titles .~ map serieName series -- ["Cash","Equity"]
--       $ plot_bars_values .~ addIndexes (seriesToBarClusters series) -- [[20,45],[45,30],[30,20],[70,25]]
--       $ plot_bars_style .~ BarsClustered
--       $ plot_bars_spacing .~ BarsFixGap 30 5
--       $ plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq)
--       $ def

--   alabels = categories -- [ "test" ]

--   bstyle = Just (solidLine 1.0 $ opaque black)
--   mkstyle c = (solidFillStyle c, bstyle)


-- --doBars :: [Serie] -> String -> String -> String -> SOMETHING!!!
-- doBars series title xlabel ylabel = layout
--   where
--     layout =
--         layout_title .~ title
--       $ layout_title_style . font_size .~ 10
--       $ layout_x_axis . laxis_generate .~ autoIndexAxis (map serieName series)
--       $ layout_y_axis . laxis_override .~ axisGridHide
--       $ layout_left_axis_visibility . axis_show_ticks .~ False
--       $ layout_plots .~ [ plotBars bars ]
--       $ def :: Layout PlotIndex Double

    
--     bars =
--         plot_bars_titles .~ []
--       $ plot_bars_values .~ addIndexes (seriesToBars series) -- [[20,45],[45,30],[30,20],[70,25]]
--       $ plot_bars_style .~ C.BarsClustered
--       $ plot_bars_spacing .~ BarsFixGap 30 5
--       $ plot_bars_item_styles .~ map mkstyle (repeat (head (cycle defaultColorSeq)))
--       $ def

--     bstyle = Just (solidLine 1.0 $ opaque black)
--     mkstyle c = (solidFillStyle c, bstyle)


-- ------------------------------------------------------------------------------
-- -- Plot lines
-- -- doLines :: [Serie] -> String -> String -> String -> IO ()
-- doLines = undefined



-- ---------------------------------------------------------------------------
-- -- Recognize data


-- -- The rules.
-- -- The string contains only numerals -> Int
-- -- The string contains only numerals and exactly one . -> Double
-- -- the string contains exactly one . and an e -> Double 
-- -- The string contains any non-number char -> String

-- -- there is an ordering to the rules.
-- -- # 1 If any element of the
-- --     input contains something that implies String. They are all interpreted as Strings
-- -- # 2 If not #1 and any element contains . or . and e All values are doubles
-- -- # 3 If not #1 and #2 treat as Int

-- -- May be useless. just treat all values as "Double" 

-- -- | Figure out what type of value is stored in this data series.     
recogValueType :: String -> ValueType
recogValueType str =
  case (isString str, isInt str, isDouble str) of
    (True,_,_)     -> String
    (False,True, False)  -> Int
    (False,_, True)  -> Double
    (_, _, _) -> String
  where 
    isInt str = all isNumber str
    -- Not a very proper check. 
    isDouble str = ((all isNumber $ delete '.' str)
                    || (all isNumber $ delete '.' $ delete 'e' str)
                    || (all isNumber $ delete '.' $ delete 'e' $ delete '-' str))
                   && '.' `elem` str
    isString str = not (isInt str) && not (isDouble str)


-- ---------------------------------------------------------------------------
-- -- Try Sorting as numbers

-- trySortAsNum :: [String] -> Maybe [String]
-- trySortAsNum str =
--   case valueType of
--     -- If they are Integers I dont see how this can fail
--     Int ->  Just $ map show $ sort $ (map read str :: [Int])
--     -- If they are doubles, mae sure read/show invariant holds
--     Double ->
--       let sorted = map show $ sort $ (map read str :: [Double])
--       in if (all (\x -> elem x sorted) str) then Just sorted else Nothing

--     -- If they are words. Well, could sort, but doing nothing. 
--     String -> Nothing 
      
--   where 
--     valueType = recogValueType str 





-- ---------------------------------------------------------------------------
-- -- apply the key. that is move some columns to the "front" and concatenate
-- -- their contents

-- applyKey :: [Flag] -> [[String]] -> [[String]]
-- applyKey flags csv =
--   if keyActive
--   then map doIt csv
       
       
--   else csv 
--   where
--     keyActive = (not . null) [() | Key _ <- flags]
--     key = head [read x :: [Int] |  Key x <- flags]

--     key_sorted_reverse = reverse $ sort key 

--     fix_row_head r = concatMap (\i -> r !! i) key
--     fix_row_tail r = dropCols key_sorted_reverse r

--     doIt r = fix_row_head r : fix_row_tail r 

--     dropCol x r = take x r ++ drop (x+1) r
--     dropCols [] r = r 
--     dropCols (x:xs) r = dropCols xs (dropCol x r)
    
       
        
-- Probably not very flexible. More thinking needed
    
-- applyGroup :: [Flag] -> [[String]] -> [[String]]
-- applyGroup flags csv = 
--   if groupActive
--   then
--     --error $ "\n\n" ++ show csv ++ "\n\n" ++ show theGroups ++
--     --        "\n\n" ++ show theNames ++ "\n\n" ++ show allGroups ++
--     --        "\n\n" ++ show doIt 
            
--     case groupingIsValid of
--       True -> doIt -- groupBy csv
--       False -> error "Current limitation is that a table needs exactly three fields to apply grouping"
--   else csv

--   where
--     groupActive = (not . null) [() | GroupBy _ <- flags]
--     groupBy = head [read x :: Int |  GroupBy x <- flags]

--     getKey r = head r

--     groupingIsValid = all (\r -> length r == 3) csv

--     theGroups' = nub $ sort $  map (\r -> r !! groupBy) csv 
--     theGroups  =
--       case trySortAsNum theGroups' of
--         Nothing -> theGroups
--         Just sorted -> sorted 

--     --- Ooh dangerous!!! 
--     valueIx = head $ delete groupBy [1,2]
    
--     -- makes unreasonable assumptions! (ordering) 
--     -- some sorting needs to be built in for the general case. 
--     theNames = nub $ map head csv 

--     --extractValues g rows = [r !! valueIx | r <- rows
--      --                                    , g == (r !! groupBy)]

--     allGroups = map (\n -> extractValues n csv) theNames --theGroups


--     -- for each NAME. Pull out all values 
--     extractValues name rows = organize theGroups $ [(r !! groupBy, r !! valueIx) | r <- rows ,getKey r == name] 

--     --organize the values in the order specified by "theGroups"
--     organize [] [] = []
--     organize [] xs  = error $ show xs
--     organize (x:xs) ys = 
--       case lookup x ys of
--         Nothing -> error "applyGroup: Bug alert!"
--         Just y  -> y: (organize xs (deleteBy (\(a,_) (b,_) -> a == b) (x,"") ys) )
    
    
--     doIt = ("KEY" : theGroups) : zipWith (\a b -> a : b) theNames allGroups 

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
-- applyGroup :: [Flag] -> [[String]] -> [[String]]
-- applyGroup flags csv = 
--   if groupActive
--   then
            
--     case groupingIsValid of
--       True -> doIt -- groupBy csv
--       False -> error "Current limitation is that a table needs exactly three fields to apply grouping"
--   else csv

--   where
--     groupActive = (not . null) [() | GroupBy _ <- flags]
--     -- This could also potentially be a name, right ? 
--     groupSpecifierColumn = head [read x :: Int |  GroupBy x <- flags]

--     -- After rearrangement the key will be at the head of the list
--     -- (Thats why grouping requires keying) 
--     getKey r = head r

--     -- I have no idea what to do if the table has too many column,
--     -- Could require that the user specify Key, Group and Value column,
--     -- and filter out those in the process of grouping. 
--     groupingIsValid = all (\r -> length r == 3) csv

--     -- Read out all different values in the groupSpecifierColumn.
--     theGroups' = nub $ sort $  map (\r -> r !! groupSpecifierColumn) csv 
--     theGroups  =
--       case trySortAsNum theGroups' of
--         Nothing -> theGroups
--         Just sorted -> sorted 

--     --- Ooh dangerous!!!
--     --  But should work if key is in column 0 (user has rekeyed) 
--     --  and if number of columns is 3. 
--     valueIx = head $ delete groupSpecifyerColumn [1,2]
    

--     -- extract all the different keys.
--     -- Could try to sort these as numbers as well.. 
--     allKeys = nub $ sort $ map head csv 

--     allGroups = map (\n -> extractValues n csv) theNames --theGroups


--     -- for each NAME. Pull out all values 
--     extractValues name rows = organize theGroups $ [(r !! groupBy, r !! valueIx) | r <- rows ,getKey r == name] 

--     --organize the values in the order specified by "theGroups"
--     organize [] [] = []
--     organize [] xs  = error $ show xs
--     organize (x:xs) ys = 
--       case lookup x ys of
--         Nothing -> error "applyGroup: Bug alert!"
--         Just y  -> y: (organize xs (deleteBy (\(a,_) (b,_) -> a == b) (x,"") ys) )
    
    
--     doIt = ("KEY" : theGroups) : zipWith (\a b -> a : b) theNames allGroups 


--     -- Can potentially find a whole list of values at a given key
--     -- and a given groupID
--     extractGroup :: (String,Int) -> (String,Int) -> [[String]] -> (String,(String, [String]))
--     extractGroup (key,keyix) (groupElem,gix) table = undefined 
