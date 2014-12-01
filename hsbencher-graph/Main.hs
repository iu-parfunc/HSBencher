{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Environment (getArgs, getEnv, getEnvironment)
import System.Console.GetOpt (getOpt', ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.IO (Handle, hPutStrLn, stderr,stdin, openFile, hClose, hGetContents,
                  hIsEOF, hGetLine, IOMode(..), BufferMode(..), hSetBuffering, withFile)

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

{- Random junk
cabal install HSBencher/hsbencher-tool/ HSBencher/hsbencher-graph/ --bindir=/home/joels/Current/ObsidianWriting/Winter2014/bin/ --force-reinstalls
cat ./text/data/Scan-cse-324974-663.csv | ./bin/grapher -o apa.png --key="ARGS0" --key="ARGS1" -x ARGS2 -y runtime

-} 

-- 
---------------------------------------------------------------------------

-- | Command line flags to the benchmarking executable.
data Flag = ShowHelp | ShowVersion
          | File String       -- files must have same format
                              -- and same format as any csv arriving in pipe
          | OutFile String
          | RenderMode GraphMode
          | Title String
          | XLabel String
          | YLabel String

          -- identify part of a key 
          | Key String -- --key="Arg1" --key?"Arg2" means Arg1_Arg2 is the name of data series
          | XValues String -- column containing x-values
          | YValues String -- column containing y-values

          -- output resolution  
          | XRes String
          | YRes String

          -- output format
          | OutFormat MyFileFormat

          -- The standard use case is as a pipe "|"
          | NoPipe

 -- This is getting messy 
            -- Normalize against this column
          | NormalizeKey String
          | NormalizeVal String 

          -- Aux values are not plotted!
          | AuxFile String -- a single auxfile that need not have same format as other CSV
          | AuxKey  String -- part of key into auxfile
          | AuxX    String -- column with aux x value 
          | AuxY    String -- column with aux y value 

       
            
  deriving (Eq,Ord,Show,Read)

data MyFileFormat = MySVG | MyPNG | MyPDF | MyPS
                  deriving (Eq, Ord, Show, Read) 

convToFileFormat :: MyFileFormat -> FileFormat
convToFileFormat MySVG = SVG
convToFileFormat MyPDF = PDF
convToFileFormat MyPNG = PNG
convToFileFormat MyPS = PS

data GraphMode = Bars | BarClusters | Lines 
               deriving (Eq, Ord, Show, Read )

-- | Type of values stored in a series
data ValueType = Int | Double | String 
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
     , Option ['f'] ["file"] (ReqArg File "FileName.csv")    "Use CSV file as input"
     , Option ['o'] ["out"]  (ReqArg OutFile "FileName.png") "Chart result file"
     , Option []    ["bars"] (NoArg (RenderMode Bars))       "Plot data as bars"
     , Option []    ["barclusters"] (NoArg (RenderMode BarClusters)) "Plot data as bar clusters" 
     , Option []    ["lines"] (NoArg (RenderMode Lines))     "Plot data as lines"
     , Option []    ["title"] (ReqArg Title "String")        "Plot title" 
     , Option []    ["xlabel"] (ReqArg XLabel "String")      "X-axis label"
     , Option []    ["ylabel"] (ReqArg YLabel "String")      "Y-axis label"
     , Option []    ["key"]    (ReqArg Key "String")         "Columns that make part of the key"
     , Option ['x'] ["xvalue"] (ReqArg XValues "String")      "Column containing x values"
     , Option ['y'] ["yvalue"] (ReqArg YValues "String")      "Column containing y values"

     , Option []    ["xres"]   (ReqArg XRes "String")    "X-resolution of output graphics"
     , Option []    ["yres"]   (ReqArg XRes "String")    "Y-resolution of output graphics"
     , Option []    ["SVG"]    (NoArg (OutFormat MySVG)) "Output in SVG format"
     , Option []    ["PDF"]    (NoArg (OutFormat MyPDF)) "Output in PDF format"
     , Option []    ["PS"]     (NoArg (OutFormat MyPS))  "Output in PS format"
     , Option []    ["PNG"]    (NoArg (OutFormat MyPNG)) "Output in PNG format"

     , Option []    ["nopipe"] (NoArg NoPipe)            "Tool is not used in a pipe"
     ]

-- | Multiple lines of usage info help docs.
fullUsageInfo :: String
fullUsageInfo = usageInfo docs core_cli_options
 where 
  docs = "USAGE: grapher <flags> ...\n"++
         "\n\nhsbencher-graph general options: \n"
--   ++ generalUsageStr

---------------------------------------------------------------------------
-- Data representations
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

---------------------------------------------------------------------------
-- Data series
---------------------------------------------------------------------------
  
type DataSeries = M.Map String [(SeriesData,SeriesData)] 

insertVal :: DataSeries -> String -> (SeriesData,SeriesData) -> DataSeries
insertVal m key val@(x,y) =
  case M.lookup key m of
    Nothing -> M.insert key [val] m 
    Just vals -> M.insert key (val:vals) m

    
---------------------------------------------------------------------------
-- MAIN
---------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs

  let (options,plainargs,_unrec,errs) = getOpt' Permute core_cli_options args

      outputSpecified = (not . null) [() | OutFile _ <- options]
      outputFormatSpecified = (not . null) [() | OutFormat _ <- options]

      outFormat = head $ [convToFileFormat format | OutFormat format <- options] ++ [PNG] 

      inFiles = [nom | File nom <- options]   
  
      xRes = head $ [read x | XRes x <- options] ++ [800]
      yRes = head $ [read y | YRes y <- options] ++ [600]
  
      plotTitle = head $ [t | Title t <- options] ++ ["NO_TITLE"]
      xLabel    = head $ [l | XLabel l <- options] ++ ["X-Axis"] 
      yLabel    = head $ [l | YLabel l <- options] ++ ["Y-Axis"]
      
      outfile = head [nom | OutFile nom <- options]
  
      -- The user specified not to pipe ?
      nopipe = (not . null) [() | NoPipe <- options]
      
      
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
  -- Get target
  let outFile = head [file | OutFile file <- options]  

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
  
  csv' <- case nopipe of 
            False -> getCSV key xy
            True  -> return $ M.empty 
  
  ---------------------------------------------------------------------------
  -- read aux csv from files
  putStrLn $ show inFiles 

  csv <- readCSVFiles csv' inFiles key xy 
  
         
  hPutStrLn stderr $ "printing csv" 
  hPutStrLn stderr $ show csv

  ---------------------------------------------------------------------------
  -- Create a lineplot 

  let series :: [(String,[(SeriesData,SeriesData)])]
      series = M.assocs csv

      series' = map unifyTypes series
      series_type = typecheck series' 
      outResolution = (xRes,yRes) 
  
  case series_type of
    Just (Int,Int) -> plotIntInt outFile outFormat outResolution series'
    Just (Int,Double) -> plotIntDouble outFile outFormat outResolution series'
    Just (Double,Double) -> plotDoubleDouble outFile outFormat outResolution series'
    Just (_,_) -> error $ "no support for plotting of this series type: " ++ show series_type
    Nothing -> error $ "Series failed to typecheck" 
  
---------------------------------------------------------------------------
-- Plotting
    
plotIntInt outfile series = undefined


plotIntDouble outfile outFormat outResolution series = do 
  let fopts = FileOptions outResolution outFormat 
  toFile fopts outfile $ do
    
    layout_title .= "testplot from grapher"
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= (opaque black)
    layout_left_axis_visibility . axis_show_ticks .= True
    layout_title_style . font_size .= 24
    

    mapM_ plotIt series 
  
  where
    plotIt (name,xys) = do
      color <- takeColor
      shape <- takeShape

      let sorted = (sortBy (\(x,_) (x',_) -> x `compare` x')  $ zip xsi ysd)
      plot $ myline color name [sorted]
      plot $ mypoints color shape name sorted
      where 
        (xs,ys)  =  unzip xys
        xsi = map fromData xs :: [Int]
        ysd = map fromData ys :: [Double]

    myline :: AlphaColour Double -> String -> [[(x,y)]]  -> EC l (PlotLines x y)
    myline color title values = liftEC $ do
      plot_lines_title .= title
      plot_lines_values .= values
      plot_lines_style . line_color .= color
      plot_lines_style . line_width .= 5


    mypoints :: AlphaColour Double -> PointShape -> String -> [(x,y)] -> EC l (PlotPoints x y)
    mypoints color shape name values = liftEC $ do
      plot_points_values .= values
      plot_points_title .= name
      plot_points_style . point_color .= transparent 
      plot_points_style . point_shape .= shape
      plot_points_style . point_border_width .= 4
      plot_points_style . point_border_color .= color
      plot_points_style . point_radius .= 8

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

getCSV :: [String] -> (String,String) -> IO DataSeries
getCSV = getCSVHandle M.empty stdin


readCSVFiles :: DataSeries -> [FilePath] -> [String] -> (String,String) -> IO DataSeries 
readCSVFiles m [] _ _ = return m
readCSVFiles m (f:fs) keys xy =
  do m' <-
       withFile f ReadMode $ \h ->
            getCSVHandle m h keys xy
     readCSVFiles m' fs keys xy 

     
getCSVHandle :: DataSeries -> Handle -> [String] -> (String,String) -> IO DataSeries --[[String]]
getCSVHandle m handle keys (xcol,ycol)= do
  -- Read of the headers 
  res <- catch
             (do
                 s <- hGetLine handle
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
          xcolIx =
            case elemIndex xcol csv of
              Just ix -> ix
              Nothing -> error $ show xcol ++ " is not present in csv." 
          ycolIx =
            case elemIndex ycol csv of
              Just ix -> ix
              Nothing -> error $ show ycol ++ " is not present in csv."
          

          
  --    putStrLn $ "Specified key: " ++ keyStr    
  --    putStrLn $ "Key Indices:   " ++ show keyIxs
  --    putStrLn $ "Read the headers line: " ++ str
      
        
      case (length keyIxs) == (length keys) of
        False -> error "Key is not part of table"
        True -> do
          m' <- loop keyIxs (xcolIx,ycolIx) m
  --        putStrLn $ show m
          return m'
  where
    loop keyIxs xy m = do
      res <- catch
             (do
                 s <- hGetLine handle
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

