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

-- | Command line flags to the executable.
data Flag = ShowHelp | ShowVersion
          | File String       -- files must have same CSV layout
                              -- and same format as any CSV arriving in pipe
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
          | NormaliseKey String
          | NormaliseVal String 

          
            
          --   -- Aux values are not plotted!
          -- | AuxFile String -- a single auxfile that need not have same format as other CSV
          -- | AuxKey  String -- part of key into auxfile
          -- | AuxX    String -- column with aux x value 
          -- | AuxY    String -- column with aux y value 
  deriving (Eq,Ord,Show,Read)

-- This is all a bit unfortunate. 
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


-- Once things start to work more properly, exception handling should be made proper. 
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
       
     , Option []    ["normalise"] (ReqArg NormaliseKey "String") "What value to normalise against" 
     ]

-- | Multiple lines of usage info help docs.
fullUsageInfo :: String
fullUsageInfo = usageInfo docs core_cli_options
 where 
  docs = "USAGE: hsbencher-graph <flags> ...\n"++
         "\nA utility for plotting datasets retrieved from HSBencher.\n"++
         "\nCommand line flags: \n"
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
-- Plot configuration
---------------------------------------------------------------------------
data PlotConfig = PlotConfig { plotOutFile    :: FilePath
                             , plotOutFormat  :: FileFormat
                             , plotResolution :: (Int,Int)                                
                             , plotTitle      :: String                              
                             , plotXLabel     :: String
                             , plotYLabel     :: String
                             }

---------------------------------------------------------------------------
-- MAIN
---------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs

  let (options,plainargs,_unrec,errs) = getOpt' Permute core_cli_options args

      outputSpecified = (not . null) [() | OutFile _ <- options]
      outputFormatSpecified = (not . null) [() | OutFormat _ <- options]

      normaliseSpecified = (not . null) [ () | NormaliseKey _ <- options] 
  
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

  --------------------------------------------------
  -- Get target
  let outFile = head [file | OutFile file <- options]  

  --------------------------------------------------
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
      
  --------------------------------------------------
  -- read csv from stdin. 
  
  (csv',aux') <- case nopipe of 
    False -> getCSV key xy
    True  -> return $ (M.empty, M.empty)
  
  --------------------------------------------------
  -- read aux csv from files
  putStrLn $ show inFiles 

  (csv,aux) <- readCSVFiles (csv',aux') inFiles key xy 
  
         
  hPutStrLn stderr $ "printing csv" 
  hPutStrLn stderr $ show csv


  hPutStrLn stderr $ "printing aux" 
  hPutStrLn stderr $ show aux

  --------------------------------------------------
  -- Create a lineplot 

  let series :: [(String,[(SeriesData,SeriesData)])]
      series = M.assocs csv

      series' = map unifyTypes series
      series_type = typecheck series' 

      -- normalise values against this datapoint
      normKey = head [nom | NormaliseKey nom <- options]   
      base = getBaseVal normKey csv aux 
      
      
      plot_series = if normaliseSpecified       
                    then normalise base series'
                    else series'
  --------------------------------------------------
  -- All the collected parameters for the plotting. 
  let plotConf = PlotConfig outFile
                            outFormat
                            (xRes,yRes)
                            plotTitle
                            "X-Axis"
                            "Y-Axis" 
  
  
  --------------------------------------------------
  -- do it 
  case series_type of
    Just (Int,Int) -> plotIntInt plotConf plot_series
    Just (Int,Double) -> plotIntDouble plotConf plot_series
    Just (Double,Double) -> plotDoubleDouble plotConf plot_series
    Just (_,_) -> error $ "no support for plotting of this series type: " ++ show series_type
    Nothing -> error $ "Series failed to typecheck" 
  
---------------------------------------------------------------------------
-- Plotting
    
plotIntInt conf series = undefined


--plotIntDouble outfile plotTitle outFormat outResolution series = do
plotIntDouble conf  series = do 
  let fopts = FileOptions (plotResolution conf)
                          (plotOutFormat conf)
  toFile fopts (plotOutFile conf) $ do
    
    layout_title .= plotTitle conf
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= opaque black
    layout_left_axis_visibility . axis_show_ticks .= True
    layout_title_style . font_size .= 24
    
    mapM_ plotIt series 
  
  where
    plotIt (name,xys) = do
      color <- takeColor
      shape <- takeShape

      let (xs,ys) = unzip xys
          xsi     = map fromData xs :: [Int]
          ysd     = map fromData ys :: [Double]
          sorted  = (sortBy (\(x,_) (x',_) -> x `compare` x')  $ zip xsi ysd)
          
      plot $ myline color name [sorted]
      plot $ mypoints color shape name sorted
        
    myline :: AlphaColour Double -> String -> [[(x,y)]]  -> EC l (PlotLines x y)
    myline color title values = liftEC $ do
      plot_lines_title .= title
      plot_lines_values .= values
      plot_lines_style . line_color .= color
      plot_lines_style . line_width .= 1


    mypoints :: AlphaColour Double -> PointShape -> String -> [(x,y)] -> EC l (PlotPoints x y)
    mypoints color shape name values = liftEC $ do
      plot_points_values .= values
      plot_points_title .= name
      plot_points_style . point_color .= transparent 
      plot_points_style . point_shape .= shape
      plot_points_style . point_border_width .= 1
      plot_points_style . point_border_color .= color
      plot_points_style . point_radius .= 2

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

getCSV :: [String] -> (String,String) -> IO (DataSeries,DataSeries)
getCSV = getCSVHandle (M.empty,M.empty) stdin


readCSVFiles :: (DataSeries,DataSeries) -> [FilePath] -> [String] -> (String,String) -> IO (DataSeries,DataSeries)
readCSVFiles m [] _ _ = return m
readCSVFiles m (f:fs) keys xy =
  do m' <-
       withFile f ReadMode $ \h ->
            getCSVHandle m h keys xy
     readCSVFiles m' fs keys xy 


-- This function needs a few comments..     
getCSVHandle :: (DataSeries,DataSeries) -> Handle -> [String] -> (String,String) -> IO (DataSeries,DataSeries)
getCSVHandle (m,aux) handle keys (xcol,ycol)= do
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
          
          xcolIx =
            case elemIndex xcol csv of
              Just ix -> ix
              Nothing -> error $ show xcol ++ " is not present in csv." 
          ycolIx =
            case elemIndex ycol csv of
              Just ix -> ix
              Nothing -> error $ show ycol ++ " is not present in csv."
        
      case (length keyIxs) == (length keys) of
        False -> error $ "Keys "++ show keys++" were not all found in schema: "++show csv
        True -> do
          (m',aux') <- loop keyIxs (xcolIx,ycolIx) (m,aux)
          return (m',aux')
  where
    loop keyIxs xy (m,aux) = do
      res <- catch
             (do
                 s <- hGetLine handle
                 return $ Just s
             )
             (\(IOError _ _ _ _ _ _ )-> return Nothing)
      case res of
        -- In the odd event that an empty line occurs 
        Just []  -> loop keyIxs xy (m,aux)
        -- A real string, lets see if it contains anything useful. 
        Just str -> do
          -- split out the csv fields 
          let csv = map strip $ splitOn "," str

              -- Construct a key
              key = collectKey keyIxs csv      

              -- Find x,y pairs
              (xStr,yStr)  = collectXY xy csv
          -- empty string at key position. 
          -- May be of importance! 
          case (xStr,yStr) of
            ("","") -> do putStrLn $ "has no x/y values: " ++ show key ++ " discarding."
                          loop keyIxs xy (m,aux)
            ("",a)  -> do putStrLn $ "has no x value: " ++ show key ++ " Goes into aux data."
                          let aux' = insertVal aux key (StringData "NO_X_VALUE",
                                                        toSeriesData a)
                          loop keyIxs xy (m,aux')
            (a,"")  -> do putStrLn $ "has no y value: " ++ show key ++ " discarding."
                          loop keyIxs xy (m,aux)
            (x,y)   -> let m' = insertVal m key (toSeriesData x,
                                                 toSeriesData y)
                       in  loop keyIxs xy (m',aux)
        -- DONE! (EOF)                   
        Nothing -> return (m,aux)
        
    collectKey ixs csv = concat $ intersperse "_" $ filter (/="") $ map (\i -> csv !! i) ixs
    
    collectXY (x,y) csv =  ( collectVal x csv
                           , collectVal y csv)
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




---------------------------------------------------------------------------
-- get a value to normalise against 
---------------------------------------------------------------------------

getBaseVal normKey csv aux =
  case (M.lookup normKey csv, M.lookup normKey aux) of
    (Nothing, Nothing) -> error "Value to normalise against not found"
    (Just v,_) -> v
    (_,Just v) -> v
      


normalise :: [(SeriesData,SeriesData)] -> [(String,[(SeriesData,SeriesData)])] -> [(String,[(SeriesData,SeriesData)])]
normalise []   _  = error "No Value to normalise against"
normalise base [] = []
normalise base ((nom,serie):rest) 
  = (nom,normalise' base serie):normalise base rest
  where
    normalise' ::  [(SeriesData,SeriesData)] ->  [(SeriesData,SeriesData)] ->  [(SeriesData,SeriesData)] 
    normalise' base [] = []
    normalise' base ((sx,sy):rest) =
      doIt base (sx,sy) : normalise' base rest
    doIt ((x,NumData y):_) (sx,NumData sy) = (sx,NumData ((sy-y)/y))  
  

