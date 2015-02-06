{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Exception
-- import Control.Lens
import Control.Monad (unless,when)
import Data.Char (isNumber)
-- import Data.Colour
-- import Data.Colour.Names
-- import Data.Default.Class
import Data.List (elemIndex, intersperse, delete)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.String.Utils (strip)
import qualified Data.Set as S
import Data.Typeable
import GHC.IO.Exception (IOException(..))
import Prelude hiding (init) 
import System.Console.GetOpt (getOpt', ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (Handle, hPutStrLn, stderr,stdin, hGetLine, IOMode(..),  withFile)
import qualified Text.CSV as CSV

#ifdef USECHART
-- Charting library
import Graphics.Rendering.Chart as C
import Graphics.Rendering.Chart as C 
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo
import Graphics.Rendering.Chart.Easy as C
#endif

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

          -- Log mode
          | YLog  -- logarithmic scale on y-axis
          | XLog  -- logarithmic scale on x-axis 

          -- identify part of a key 
          | Key String -- --key="Arg1" --key="Arg2" means Arg1_Arg2 is the name of data series
          | XValues String -- column containing x-values
          | YValues String -- column containing y-values

          -- output resolution  
          | XRes String
          | YRes String

          -- output format
          | OutFormat MyFileFormat


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
data MyFileFormat = MySVG | MyPNG | MyPDF | MyPS | MyCSV
                  deriving (Eq, Ord, Show, Read) 

#ifdef USECHART
convToFileFormat :: MyFileFormat -> Cairo.FileFormat
convToFileFormat MySVG = Cairo.SVG
convToFileFormat MyPDF = Cairo.PDF
convToFileFormat MyPNG = Cairo.PNG
convToFileFormat MyPS  = Cairo.PS
convToFileFormat MyCSV = error "Cairo does not do CSV output"
#endif

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

     -- Logarithmic scales
     , Option []     ["ylog"] (NoArg YLog)                "Logarithmic scale on y-axis"
     , Option []     ["xlog"] (NoArg XLog)                "Logarithmix scale on x-axis" 
       
     -- plot configuration 
     , Option []    ["xres"]   (ReqArg XRes "String")    "X-resolution of output graphics"
     , Option []    ["yres"]   (ReqArg XRes "String")    "Y-resolution of output graphics"
     , Option []    ["SVG"]    (NoArg (OutFormat MySVG)) "Output in SVG format"
     , Option []    ["PDF"]    (NoArg (OutFormat MyPDF)) "Output in PDF format"
     , Option []    ["PS"]     (NoArg (OutFormat MyPS))  "Output in PS format"
     , Option []    ["PNG"]    (NoArg (OutFormat MyPNG)) "Output in PNG format"

     , Option []    ["normalise"] (ReqArg NormaliseKey "String") "What value to normalise against" 
     ]

-- | Multiple lines of usage info help docs.
fullUsageInfo :: String
fullUsageInfo = usageInfo docs core_cli_options
 where 
  docs = "USAGE: hsbencher-graph <flags> ...\n"++
         "\nA utility for plotting datasets retrieved from HSBencher.\n"++
         "\nReads CSV data from stdin unless --file is given.\n"++         
         "\nCommand line flags: \n"
--   ++ generalUsageStr

---------------------------------------------------------------------------
-- Data representations
---------------------------------------------------------------------------
data SeriesData = IntData Int 
                | NumData Double
                | StringData String
                  deriving (Show, Eq, Ord, Read)

convertToString :: SeriesData -> String
convertToString (IntData x) = (show x)
convertToString (NumData x) = (show x)
convertToString (StringData a) = a

isNum :: SeriesData -> Bool
isNum (NumData _) = True
isNum _ = False

seriesType :: SeriesData -> ValueType
seriesType (IntData _) = Int
seriesType (NumData _) = Double
seriesType (StringData _) = String 

class FromData a where
  fromData :: SeriesData -> a

instance FromData Double where
  fromData (NumData a) = a
  fromData (IntData n) = fromIntegral n -- RRN: TEMP: sanity check this.  Allow Int to "subtype" as double.
  fromData e = error $ "FromData Double, could not parse as Double: "++show e

instance FromData Int where
  fromData (IntData a) = a
  fromData e = error$ "FromData Int, could not parse: "++show e

instance FromData String where
  fromData (StringData a) = a
  fromData e = error "FromData String, could not parse: "++show e 

---------------------------------------------------------------------------
-- Data series
---------------------------------------------------------------------------
  
type DataSeries = M.Map String [(SeriesData,SeriesData)] 

insertVal :: DataSeries -> String -> (SeriesData,SeriesData) -> DataSeries
insertVal m key val@(_x,_y) =
  case M.lookup key m of
    Nothing -> M.insert key [val] m 
    Just vals -> M.insert key (val:vals) m


---------------------------------------------------------------------------
-- Plot configuration
---------------------------------------------------------------------------
data PlotConfig = PlotConfig { plotOutFile    :: FilePath
                             , plotOutFormat  :: MyFileFormat 
                             , plotResolution :: (Int,Int)                                
                             , plotTitle      :: String                              
                             , plotXLabel     :: String
                             , plotYLabel     :: String
                             , plotYLog       :: Bool
                             , plotXLog       :: Bool
                             }
                  deriving (Show, Eq, Read, Ord)

chatter :: String -> IO ()
chatter str = hPutStrLn stderr $ " [hsbencher-graph] " ++str

---------------------------------------------------------------------------
-- MAIN
---------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs

  let (options, _plainargs, _unrec,errs) = getOpt' Permute core_cli_options args

      outputSpecified = (not . null) [() | OutFile _ <- options]
      _outputFormatSpecified = (not . null) [() | OutFormat _ <- options]

      normaliseSpecified = (not . null) [ () | NormaliseKey _ <- options] 
  
      inFiles = [nom | File nom <- options]   
  
      xRes = head $ [read x | XRes x <- options] ++ [800]
      yRes = head $ [read y | YRes y <- options] ++ [600]
  
      plotTitle = head $ [t | Title t <- options] ++ ["NO_TITLE"]
      xLabel    = head $ [l | XLabel l <- options] ++
                         [c | XValues c <- options] ++
                         ["X-Axis"] -- Last resort, default
      yLabel    = head $ [l | YLabel l <- options] ++
                         [c | YValues c <- options] ++
                         ["Y-Axis"]

      -- Should any axis be log scale
      y_logscale = (not . null) [() | YLog <- options]
      x_logscale = (not . null) [() | XLog <- options]

  outFormat <- case [ format | OutFormat format <- options] of        
                []  -> do chatter$ "Warning: no output format selected.  Defaulting to CSV output."
                          return MyCSV 
                [x] -> return x
                ls  -> error$ "multiple output formats not yet supported: "++show ls
      
  unless (null errs) $ do
    chatter$ "Errors parsing command line options:"
    mapM_ (putStr . ("   "++)) errs       
    exitFailure

  when (ShowHelp `elem` options) $ do 
    putStrLn fullUsageInfo
    exitSuccess

  when (not outputSpecified) $ do
    error $ "Error: an output file has to be specified"

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
  -- All the collected parameters for the plotting. 
  let plotConf = PlotConfig outFile
                            outFormat
                            (xRes,yRes)
                            plotTitle
                            xLabel
                            yLabel
                            y_logscale
                            x_logscale
  
  --------------------------------------------------
  -- Acquire data:

  (csv,aux) <- case inFiles of    
                [] -> getCSV key xy -- Read from stdin.
                ls -> do chatter$ "Reading CSV from input files: "++unwords ls
                         readCSVFiles (M.empty,M.empty) inFiles key xy 

  chatter$ "Here is a sample of the CSV:" ++ take 500 (show csv)
  chatter$ "Printing aux: "++ take 500 (show aux) 

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

  chatter$ "Inferred types for X/Y axes: "++show series_type  
  
  --------------------------------------------------
  -- do it      
  case (outFormat, series_type) of
    (_,Nothing) -> error $ "Series failed to typecheck" 
    (MyCSV, _)  -> do writeCSV plotConf plot_series
                      writeGnuplot plotConf plot_series  -- TEMP.  Add a command line flag for this.
#ifdef USECHART    
    (_,Just (Int,Int))       -> plotIntInt plotConf plot_series
    (_,Just (Int,Double))    -> plotIntDouble plotConf plot_series
    (_,Just (Double,Double)) -> plotDoubleDouble plotConf plot_series
#endif    
    (_,Just (_,_)) -> error $ "no support for plotting of this series type: "++show series_type++
                              ", with this output format: "++show outFormat

  
---------------------------------------------------------------------------
-- Plotting


-- | Don't produce an actual chart, rather write out the data as CSV.
writeCSV :: PlotConfig -> [(String, [(SeriesData, SeriesData)])] -> IO ()
-- Assumes a shared and sensible X axis for all data series.
writeCSV conf@PlotConfig{..} series = do
  -- ASSUMPTION: we assume a sensible Ord instance for
  -- SeriesData... that makes possible unfair assumptions about
  -- "deriving":
  let allKeys = map convertToString $
                S.toAscList $ S.fromList $ concatMap ((map fst) . snd) series
      header = plotXLabel : map fst series
      alldata = M.map (\prs -> M.fromList
                        [ (convertToString x, convertToString y) | (x,y) <- prs ] ) $ 
                M.fromList series
      rows = [ [ case M.lookup key seriesMap of
                    Nothing -> "" -- TODO, make configurable.  Missing data for this X value in this line.
                    Just x  -> x
               | (seriesName, _) <- series 
               , let seriesMap = alldata M.! seriesName ]
             | key <- allKeys ]
      csvResult = CSV.printCSV (header:rows)
  chatter $ "Writing out CSV for "++show (length series)++" data series (lines): "++unwords (tail header) 
  writeFile plotOutFile csvResult
  chatter $ "Succesfully wrote file "++show plotOutFile

  
writeGnuplot :: Show a => PlotConfig -> [(a, t)] -> IO ()
writeGnuplot conf@PlotConfig{..} series = do
  chatter $ " TEMP: write out Gnuplot script too!!  TODO: Clean up this feature."
  let gplfile = plotOutFile ++ ".gpl"      
      gplLines = [ "set xlabel "++ show plotXLabel
                 , "set ylabel "++ show plotYLabel
                 , "set output "++ show (plotOutFile ++ ".pdf")
                 , "plot "++ concat (intersperse ", "
                   -- Line them up carefully by position:
                   [ show plotOutFile++" using 1:"++show ind++" title "++show seriesName++" w lp ls "++show(ind-1)
                   | ((seriesName, _),ind) <- zip series [2::Int ..] ])
                 ]
  -- FIXME: assumes the template is in the current directory.  Use a more principled way:
  prelude <- fmap lines $ readFile "template.gpl"
  writeFile gplfile (unlines (prelude ++ gplLines))
  chatter $ "Succesfully wrote file "++show gplfile
  return ()
  

#ifdef USECHART
plotIntInt :: PlotConfig -> [(String, [(SeriesData, SeriesData)])] -> IO ()
plotIntInt conf series = error "hsbencher-graph: plotIntInt not implemented!!"

plotIntDouble :: PlotConfig -> [(String, [(SeriesData, SeriesData)])] -> IO ()
plotIntDouble conf  series = do 
  let fopts = Cairo.FileOptions (plotResolution conf)
                                (convToFileFormat (plotOutFormat conf))
  Cairo.toFile fopts (plotOutFile conf) $ do

    -- 13 colors
    setColors $ map opaque
              [blue, green, orange, red
              ,brown, black, darkblue, darkgray
              ,darkgreen, darkorange, darkred, yellow, violet]
    
    layout_title .= plotTitle conf
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= opaque black
    layout_left_axis_visibility . axis_show_ticks .= True
    layout_title_style . font_size .= 24

    if (plotYLog conf)
      then layout_y_axis . laxis_generate .= autoScaledLogAxis (LogAxisParams show) 
      else return () 


    -- Not Possible in IntDouble plot
    --if (plotXLog conf)
    --   then layout_x_axis . laxis_generate .= autoScaledLogAxis (LogAxisParams show)
    --   else return () 

    -- hint ? 
    --layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)  
            
    
    
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

plotDoubleDouble :: PlotConfig -> [(String, [(SeriesData, SeriesData)])] -> IO ()
plotDoubleDouble = error "hsbencher-graph: plotDoubleDouble not implemented!!"

#endif

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
    isString :: SeriesData -> Bool
    isString (StringData _) = True
    isString _ = False

    isInt :: SeriesData -> Bool
    isInt (IntData _) = True
    isInt _ = False 
    
    unify xs =
      case (any isString xs, any isInt xs, any isNum xs) of
        (True, _, _)   -> map (StringData . convertToString) xs
        (False,_,True) -> map convertToNum xs
        (False,True,False) -> xs
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
           _xb = all (==x) $ map seriesType xs
           _yb = all (==y) $ map seriesType ys
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


-- Read CSV data from a handle.
--   TODO: replace this with a normal CSV library? -RRN
getCSVHandle :: (DataSeries,DataSeries) -> Handle -> [String] -> (String,String) -> IO (DataSeries,DataSeries)
getCSVHandle (m0,aux0) hndl keys (xcol,ycol)= do
  -- Read of the headers 
  res <- catch
             (do
                 s <- hGetLine hndl
                 return $ Just s
             )
             (\(IOError _ _ _ _ _ _ ) -> return Nothing)
  case res of
    Nothing -> error "Error trying to get csv header line"
    Just str -> do 
      let csv = map strip $ splitOn "," str
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
          (m',aux') <- loop keyIxs (xcolIx,ycolIx) (m0,aux0)
          return (m',aux')
  where
    loop keyIxs xy (m,aux) = do
      res <- catch
             (do
                 s <- hGetLine hndl
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
            ("","") -> do chatter$ "has no x/y values: " ++ show key ++ " discarding."
                          loop keyIxs xy (m,aux)
            ("",a)  -> do chatter$ "has no x value: " ++ show key ++ " Goes into aux data."
                          let aux' = insertVal aux key (StringData "NO_X_VALUE",
                                                        toSeriesData a)
                          loop keyIxs xy (m,aux')
            (_a,"") -> do chatter$ "has no y value: " ++ show key ++ " discarding."
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
    (True,_,_)           -> String
    (False,True, False)  -> Int
    (False,_, True)      -> Double
    (_, _, _)            -> String
  where 
    isInt s = all isNumber s
    -- Not a very proper check. 
    isDouble s = ((all isNumber $ delete '.' s)
                    || (all isNumber $ delete '.' $ delete 'e' s)
                    || (all isNumber $ delete '.' $ delete 'e' $ delete '-' s))
                   && '.' `elem` s
    isString s = not (isInt s) && not (isDouble s)




---------------------------------------------------------------------------
-- get a value to normalise against 
---------------------------------------------------------------------------

getBaseVal :: Ord k => k -> M.Map k a -> M.Map k a -> a
getBaseVal normKey csv aux =
  case (M.lookup normKey csv, M.lookup normKey aux) of
    (Nothing, Nothing) -> error "Value to normalise against not found"
    (Just v,_) -> v
    (_,Just v) -> v
      


normalise :: [(SeriesData,SeriesData)] -> [(String,[(SeriesData,SeriesData)])] -> [(String,[(SeriesData,SeriesData)])]
normalise []   _  = error "No Value to normalise against"
normalise _ [] = []
normalise base0 ((nom,series):rest0) 
  = (nom,normalise' base0 series):normalise base0 rest0
  where
    normalise' ::  [(SeriesData,SeriesData)] ->  [(SeriesData,SeriesData)] ->  [(SeriesData,SeriesData)] 
    normalise' _ [] = []
    normalise' base ((sx,sy):rest) =
      doIt base (sx,sy) : normalise' base rest
    doIt ((_x,NumData y):_) (sx,NumData sy) = (sx,NumData ((sy-y)/y))  
  

