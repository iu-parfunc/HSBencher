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

import Data.List (isInfixOf, intersperse, delete, transpose)
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
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
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
          | GroupBy LocationSpec 
            
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
  
  unless (null errs) $ do
    putStrLn$ "Errors parsing command line options:"
    mapM_ (putStr . ("   "++)) errs       
    exitFailure

  when (ShowHelp `elem` options) $ do 
    putStrLn fullUsageInfo
    exitSuccess

  ---------------------------------------------------------------------------
  -- read csv from stdin. 

  csv <- getCSV 

  -- hPutStrLn stderr $ show csv

  -- let types = map recogValueType (map tail csv)

  -- hPutStrLn stderr $ show types


  ---------------------------------------------------------------------------
  -- Perform the task specified by the command line args

  renderPlot options csv 
    

---------------------------------------------------------------------------
-- Render a plot based on the options and the csv

renderPlot :: [Flag] -> [[String]] -> IO ()
renderPlot flags csv = 
   case plotKind of
     [] -> error "No graph mode specified"
     (Bars:_) -> doBarClusters series cates title xLabel yLabel 
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
    Rows -> map rowsToSeries csv
    Columns -> map rowsToSeries $ transpose csv 
  
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
    True -> ["category" ++ show n | n <- [0..length cates]]
    False -> cates
  where cates = tail $ head input 

-- Just a test..
seriesToBarClusters :: [Serie] -> [[Double]]
seriesToBarClusters ss = transpose the_data 
  where
    the_data = map (\(Serie _ d) -> d) ss
    


------------------------------------------------------------------------------
-- Plot bars
doBarClusters :: [Serie] -> [String] -> String -> String -> String -> IO ()
doBarClusters series categories title xlabel ylabel =
  do renderableToFile def (toRenderable layout) "example11_big.png" 
     return () 
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


------------------------------------------------------------------------------
-- Plot lines
doLines :: [Serie] -> String -> String -> String -> IO ()
doLines = undefined




-- chart borders = toRenderable layout
--  where
--   layout = 
--         layout_title .~ "Sample Bars" ++ btitle
--       $ layout_title_style . font_size .~ 10
--       $ layout_x_axis . laxis_generate .~ autoIndexAxis alabels
--       $ layout_y_axis . laxis_override .~ axisGridHide
--       $ layout_left_axis_visibility . axis_show_ticks .~ False
--       $ layout_plots .~ [ plotBars bars2 ]
--       $ def :: Layout PlotIndex Double

--   bars2 = plot_bars_titles .~ ["Cash","Equity"]
--       $ plot_bars_values .~ addIndexes [[20,45],[45,30],[30,20],[70,25]]
--       $ plot_bars_style .~ BarsClustered
--       $ plot_bars_spacing .~ BarsFixGap 30 5
--       $ plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq)
--       $ def

--   alabels = [ "Jun", "Jul", "Aug", "Sep", "Oct" ]

--   btitle = if borders then "" else " (no borders)"
--   bstyle = if borders then Just (solidLine 1.0 $ opaque black) else Nothing
--   mkstyle c = (solidFillStyle c, bstyle)


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
