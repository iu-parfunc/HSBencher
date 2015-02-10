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

import Control.DeepSeq (force)
import Data.List (elemIndex, intersperse, delete, intercalate, isInfixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.String.Utils (strip)
import qualified Data.Set as S
import Data.Typeable
import Prelude hiding (init) 
import System.Console.GetOpt (getOpt', ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
-- import System.IO (Handle, hPutStrLn, stderr,stdin, hGetLine, IOMode(..),  withFile)
import System.IO (hPutStrLn, stderr)
import System.FilePath (replaceExtension)
import qualified Text.CSV as CSV

import Debug.Trace

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

-- | CSV data where all rows have the right number of entries.
data ValidatedCSV =
     ValidatedCSV { header :: [CSV.Field]
                  , rows   :: [CSV.Record] }

-- | Groups of rows chunked together by "key"
data KeyedCSV =
     KeyedCSV { kheader :: [CSV.Field]
              , krows   :: M.Map Key [CSV.Record] }

-- | A combination of fields, e.g. A_B_C, stored as a String.
type Key = String

type ColName = String

-- | For now we only pattern match by string inclusion.  TODO: support regexps?
type Pattern = String

-- | Command line flags to the executable.
data Flag = ShowHelp | ShowVersion
          | File String       -- files must have same CSV layout
                              -- and same format as any CSV arriving in pipe
          | OutFile String
          | RenderMode GraphMode
          | Title  String
          | XLabel String
          | YLabel String

          -- Log mode
          | YLog  -- logarithmic scale on y-axis
          | XLog  -- logarithmic scale on x-axis 

          -- identify part of a key 
          | Key     { col :: ColName } -- --key="Arg1" --key="Arg2" means Arg1_Arg2 is the name of data series
          | XValues { col :: ColName } -- column containing x-values
          | YValues { col :: ColName } -- column containing y-values

          | Filter { col :: ColName, contains :: Pattern }
          | Pad    { col :: ColName, padTo :: Int }
          | Renames FilePath

          -- output resolution  
          | XRes String
          | YRes String

          -- output format
          | OutFormat MyFileFormat

          | GnuPlotTemplate FilePath

          -- Resolving/aggregating duplicates or performing comparisons:
          | Latest  { col :: ColName }
          | Speedup { col :: ColName, contains :: Pattern } 
          | Vs      { col :: ColName, contains :: Pattern }

-- This is getting messy 
            -- Normalize against this column
          | NormaliseKey { col :: ColName }
          | NormaliseVal String 

          | DummyFlag

          --   -- Aux values are not plotted!
          -- | AuxFile String -- a single auxfile that need not have same format as other CSV
          -- | AuxKey  String -- part of key into auxfile
          -- | AuxX    String -- column with aux x value 
          -- | AuxY    String -- column with aux y value 
  deriving (Eq,Ord,Show,Read)


-- This is all a bit unfortunate. 
data MyFileFormat = MySVG | MyPNG | MyPDF | MyPS
                  | MyCSV
                  | MyGPL
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

-- | For now this application is hardcoded to use a particular
-- separator in both rename files and command line arguments.
universalSeparator :: String
universalSeparator = ","

-- | Parse the comma-separated "COL,VAL" string.
parseFilter :: String -> (String,String)
parseFilter s =
  case splitOn universalSeparator s of
    [l,r] -> (l,r)
    _ -> error $ "--filter argument expected to be two strings separated by one comma, not: "++s

parsePad :: String -> (String,Int)
parsePad s =
  case splitOn universalSeparator s of
    [l,r] -> case reads r of
              [(n,"")] -> (l,n)
              _ -> error $ "--filter could not parse this as an integer: "++r
    _ -> error $ "--filter argument expected to be two strings separated by one comma, not: "++s

-- | Command line options.
core_cli_options :: [OptDescr Flag]
core_cli_options = 
     [ Option ['h'] ["help"] (NoArg ShowHelp)
        "Show this help message and exit."
     , Option ['f'] ["file"] (ReqArg File "FILE.csv")    "Use CSV file as input"
     , Option ['o'] ["out"]  (ReqArg OutFile "FILE")     "Specify result file for main output"

     , Option []    []    (NoArg DummyFlag) "\n Data Handling options"
     , Option []    []    (NoArg DummyFlag) "--------------------------------"

     , Option ['k'] ["key"]    (ReqArg Key     "STR")      "Columns that make part of the key"
     , Option ['x'] ["xvalue"] (ReqArg XValues "STR")      "Column containing x values"
     , Option ['y'] ["yvalue"] (ReqArg YValues "STR")      "Column containing y values"

     --  Not ready yet:
     , Option [] ["filter"] (ReqArg (uncurry Filter . parseFilter) "COL,VAL") $
       "Given a string \"KEY,VAL\", filter all rows to\n" ++ 
       "those where VAL is a substring of column COL."
       
     -- , Option [] ["sort"] (ReqArg () "STR") $
     --  "Name of a numeric field by which to sort the lines."

     , Option [] ["pad"] (ReqArg (uncurry Pad . parsePad) "COL,N") $
     "Treat COL as a numeric column, and pad numbers to\n"++
     "N characters, preppending leading zeros."        
       
     , Option [] ["renames"] (ReqArg Renames "FILE") $
       "Provide a comma-separated file where each line is an\n" ++
       "OLD,NEW pair indicating renames for data series' names"

     , Option []    []    (NoArg DummyFlag) "\n Resolving duplicates and making comparisons"
     , Option []    []    (NoArg DummyFlag) "----------------------------------------------"

-- TODO:
     , Option [] ["latest"] (ReqArg Latest "STR") "Grab latest data point according to monotonically increasing column."
     -- , Option [] ["speedup"] (ReqArg (uncurry Speedup . parseFilter) "KEY,VAL") $ ""
     -- , Option [] ["vs"] (ReqArg (uncurry Vs . parseFilter) "KEY,VAL") $ ""       
       
     , Option []    []    (NoArg DummyFlag) "\n Presentation options"
     , Option []    []    (NoArg DummyFlag) "--------------------------------"
       
     , Option []    ["bars"] (NoArg (RenderMode Bars))       "Plot data as bars"
     , Option []    ["barclusters"] (NoArg (RenderMode BarClusters)) "Plot data as bar clusters" 
     , Option []    ["lines"] (NoArg (RenderMode Lines))     "Plot data as lines"
     , Option []    ["title"] (ReqArg Title "String")        "Plot title" 
     , Option []    ["xlabel"] (ReqArg XLabel "String")      "X-axis label"
     , Option []    ["ylabel"] (ReqArg YLabel "String")      "Y-axis label"
       
     -- Logarithmic scales
     , Option []     ["ylog"] (NoArg YLog)                "Logarithmic scale on y-axis"
     , Option []     ["xlog"] (NoArg XLog)                "Logarithmix scale on x-axis" 

     , Option []    ["CSV"]    (NoArg (OutFormat MyCSV)) "Output raw CSV data to file selected by --out"
       
     , Option []    []    (NoArg DummyFlag) "\n Haskell Chart specific options"
     , Option []    []    (NoArg DummyFlag) "--------------------------------"
      
     -- plot configuration 
     , Option []    ["xres"]   (ReqArg XRes "String")    "X-resolution of output graphics"
     , Option []    ["yres"]   (ReqArg XRes "String")    "Y-resolution of output graphics"
     , Option []    ["SVG"]    (NoArg (OutFormat MySVG)) "Output in SVG format"
     , Option []    ["PDF"]    (NoArg (OutFormat MyPDF)) "Output in PDF format"
     , Option []    ["PS"]     (NoArg (OutFormat MyPS))  "Output in PS format"
     , Option []    ["PNG"]    (NoArg (OutFormat MyPNG)) "Output in PNG format"
      
     , Option []    []    (NoArg DummyFlag) "\n Normalisation:"
     , Option []    []    (NoArg DummyFlag) "----------------"

       
     , Option []    ["normalise"] (ReqArg NormaliseKey "String") "What value to normalise against" 
       
     , Option []    []    (NoArg DummyFlag) "\n GNUPlot Options:"
     , Option []    []    (NoArg DummyFlag) "------------------"

     , Option []    ["GPL"] (NoArg (OutFormat MyGPL)) "Output a .gpl plot script.  Implies --CSV."       
     , Option []    ["template"] (ReqArg GnuPlotTemplate "FILE") "Prepend FILE to generated gnuplot scripts."       

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

-- | These correspond to lines in the plot: named progressions of (x,y) pairs.
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
                             , gnuPlotTemplate :: Maybe FilePath
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
  
      gnuPlotTemplate = head $ [ Just f | GnuPlotTemplate f <- options] ++ [Nothing]
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
                            gnuPlotTemplate
  
  --------------------------------------------------
  -- Acquire data:

  rawdat <- case inFiles of
             []    -> error "doesn't currently support reading from stdin"
             [one] -> do chatter$ "Reading CSV from input file: "++one
                         CSV.parseCSVFromFile one
             _     -> error$ "doesn't currently support reading from multiple files: "++show inFiles

  dat0 <- case rawdat of
            Left err -> error $ "Error parsing CSV data: \n"++show err
            Right d  -> do chatter$ "Successfully parsed CSV file."
                           return d
  let dat1 = validateCSV dat0
      dat2 = doFilters [ (c,n) | Filter c n <- options ] dat1
      dat3 = doPadding [ (c,n) | Pad c n <- options ] dat2
      dat4 = takeLatest (listToMaybe [ c | Latest c <- options]) dat3
  _ <- evaluate (force (rows dat3))  -- Flush out error messages.
  
  chatter $ "Data filtering completed successfully, rows remaining: " ++ show (length (rows dat3))
  (csv,aux) <- extractData key xy dat3
  chatter $ "Data series extracted, number of lines: " ++ show (M.size csv)

  chatter$ "Here is a sample of the CSV before renaming and normalization:\n" ++
    unlines [ "    "++take 100 (show l)++"..." | l <- (take 5 (M.toList csv))] ++ "    ...."
  chatter$ "Also, a sample of 'aux': "++ take 500 (show aux) 

  --------------------------------------------------
  -- Data preprocessing / munging:

  renameTable <- fmap (concatMap lines) $
                 mapM readFile [f | Renames f <- options] 
  let series1 :: [(String,[(SeriesData,SeriesData)])]
      series1 = M.assocs csv
  
      series2 = map unifyTypes series1
      series_type = typecheck series2 

      -- normalise values against this datapoint
      normKey = head [nom | NormaliseKey nom <- options]   
      base = getBaseVal normKey csv aux 
            
      plot_series0 = if normaliseSpecified       
                     then normalise base series2
                     else series2
      renamer = buildRenamer renameTable 
      plot_series = [ (renamer nm,dat) | (nm,dat) <- plot_series0 ]
  
  chatter$ "Inferred types for X/Y axes: "++show series_type  
  
  --------------------------------------------------
  -- do it      
  case (outFormat, series_type) of
    (_,Nothing) -> error $ "Series failed to typecheck"
    (MyCSV, _)  -> writeCSV plotConf plot_series
    (MyGPL, _)  -> do chatter "Writing out both CSV file and a GnuPlot script to plot it."
                      writeCSV     plotConf plot_series
                      writeGnuplot plotConf plot_series 
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
writeCSV PlotConfig{..} series = do
  -- ASSUMPTION: we assume a sensible Ord instance for
  -- SeriesData... that makes possible unfair assumptions about
  -- "deriving":
  let allKeys = map convertToString $
                S.toAscList $ S.fromList $ concatMap ((map fst) . snd) series
      header = plotXLabel : map fst series
      alldata = M.map (\prs -> M.fromList
                        [ (convertToString x, convertToString y) | (x,y) <- prs ] ) $ 
                M.fromList series
      rows = [ key :
               [ case M.lookup key seriesMap of
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
writeGnuplot PlotConfig{..} series = do
  let gplfile = replaceExtension plotOutFile "gpl"
  chatter $ "Writing out Gnuplot script as well as CSV: "++gplfile
  let gplLines = [ "set xlabel "++ show plotXLabel
                 , "set ylabel "++ show plotYLabel
                 , "set output "++ show (replaceExtension plotOutFile "pdf")
                 , "plot "++ concat (intersperse ", "
                   -- Line them up carefully by position:
                   [ show plotOutFile++" using 1:"++show ind++" title "++show seriesName++" w lp ls "++show(ind-1)
                   | ((seriesName, _),ind) <- zip series [2::Int ..] ])
                 ]
      -- Todo, make this into a library and look up the template file in ~/.cabal/share (from the Paths_ module)
--      defaultTemplate = "template.gpl"
      defaultTemplate = error "ERROR: For now you must provide GnuPlot template with --template"
      template = fromMaybe defaultTemplate gnuPlotTemplate 
  -- FIXME: assumes the template is in the current directory.  Use a more principled way:
  prelude <- fmap lines $ readFile template
  writeFile gplfile (unlines (prelude ++ gplLines))
  chatter $ "Succesfully wrote file "++show gplfile
  return ()
  

#ifdef USECHART
plotIntInt :: PlotConfig -> [(String, [(SeriesData, SeriesData)])] -> IO ()
plotIntInt conf series = error "hsbencher-graph: plotIntInt not implemented!"

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

-- | Extract the data we care about from in-memory CSV data:
--      
-- Note: This is in the IO monad only to produce chatter.
extractData :: [String] -> (String,String) -> ValidatedCSV -> IO (DataSeries,DataSeries)
extractData keys (xcol,ycol) (ValidatedCSV header rest) = do
  let csv    = map strip header
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
      (m',aux') <- loop keyIxs (xcolIx,ycolIx) (M.empty,M.empty) rest
      return (m',aux')
  where
    loop _ _ (m,aux) [] = return (m,aux)
    loop keyIxs xy (m,aux) (row:restRows)= do
      case row of
        -- In the odd event that an empty line occurs 
        []  -> loop keyIxs xy (m,aux) restRows
        -- A real string, lets see if it contains anything useful. 
        _ -> do
          -- split out the csv fields 
          let csv = map strip row

              -- Construct a key
              key = collectKey keyIxs csv      

              -- Find x,y pairs
              (xStr,yStr)  = collectXY xy csv
          -- empty string at key position. 
          -- May be of importance! 
          case (xStr,yStr) of
            ("","") -> do chatter$ "has no x/y values: " ++ show key ++ " discarding."
                          loop keyIxs xy (m,aux) restRows
            ("",a)  -> do chatter$ "has no x value: " ++ show key ++ " Goes into aux data."
                          let aux' = insertVal aux key (StringData "NO_X_VALUE",
                                                        toSeriesData a)
                          loop keyIxs xy (m,aux') restRows
            (_a,"") -> do chatter$ "has no y value: " ++ show key ++ " discarding."
                          loop keyIxs xy (m,aux) restRows
            (x,y)   -> let m' = insertVal m key (toSeriesData x,
                                                 toSeriesData y)
                       in  loop keyIxs xy (m',aux) restRows
        
    collectKey ixs csv = concat $
                         intersperse "_" $
                         filter (/="") $
                         map (\i ->
                               -- trace ("Dereferencing !!1: "++show(csv,i)) $ 
                               csv !! i) ixs
    
    collectXY (x,y) csv =  ( collectVal x csv
                           , collectVal y csv)
    collectVal ix csv =
      -- trace ("Dereferencing !!2: "++show(csv,ix)) $
      csv !! ix 

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
  

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old

buildRenamer :: [String] -> String -> String
buildRenamer [] = id
buildRenamer (ln:rest) =
  case (splitOn "," ln) of
    [lhs,rhs] -> \str -> buildRenamer rest
                           (replace lhs rhs str)
    _ -> error ("Bad line in rename table: "++ ln)

-- | Take pre-validated CSV and apply filters to CSV data.
doFilters :: [(String,String)] -> ValidatedCSV -> ValidatedCSV
doFilters ls (ValidatedCSV header csv) =
   ValidatedCSV header $ loop ls csv
  where
   mkPrj col = 
     case elemIndex col header of
       Just ix -> \row ->
         -- trace ("Dereferencing: !!0"++show(row,ix)) $ 
         row !! ix
       Nothing -> error $ show col ++ " is not present in csv." 

   loop [] rows = rows
   loop ((col,contains):rest) rows =
     filter ((contains `isInfixOf`) . mkPrj col) $
     loop rest rows

doPadding :: [(String,Int)] -> ValidatedCSV -> ValidatedCSV
doPadding ls (ValidatedCSV header csv) =
   ValidatedCSV header $ loop ls csv
  where
   loop [] rows = rows
   loop ((col,pad):rest) rows = map (mkPadder col pad) $
                                loop rest rows
   mkPadder :: ColName -> Int -> CSV.Record -> CSV.Record
   mkPadder col padto =
     let padit s = 
           -- Make sure it is a number we are padding
          case reads s of
             [(n,"")] -> let s' = show (n::Integer)
                         in replicate (padto - length s') '0' ++ s'
             _ -> error $ "attempting to --pad something not an integer: "++s
         
           in 
     case elemIndex col header of
       Just ix -> \row ->         
         let row' = take (ix) row ++ [padit (row !! ix)] ++ drop (ix+1) row
         in -- trace ("PADDING ROW "++col++" at index "++ show ix++": "++show row')
            row'
       Nothing -> error $ show col ++ " is not present in csv." 

-- | Resolve groups of rows that share the same key
takeLatest :: Maybe ColName -> ValidatedCSV -> ValidatedCSV
takeLatest Nothing x = x
takeLatest (Just col) (ValidatedCSV header csv) =
  error "takeLatest: unfinished"

-- | Make sure that each row has the right number of columns ad discard blank lines:
validateCSV :: CSV.CSV -> ValidatedCSV
validateCSV [] = error "validateCSV: empty CSV data (no header row)"
validateCSV (header:csv) = ValidatedCSV header (loop (2::Int) csv)
  where
   numCols = length header -- TODO: could validate that column names look
                           -- right, i.e. probably shouldn't be numbers!
   loop _ [] = []
   loop p ([]:rest) = loop (p+1) rest -- Discard blank
   -- Ok, this is kind of weird... Text.CSV parses a trailing blank line
   -- as having one field of zero size:
   loop p ([""]:rest) = loop (p+1) rest 
   loop pos (row:rest)
     | length row == numCols = row : loop (pos+1) rest
     | otherwise = error $ "error in validateCSV: encountered on row #"++ show pos
                   ++ "\nRow did not contain the expected number of columns: "++show numCols
                   ++"\nCSV schema was:\n  "++show header
                   ++"\nOffending row (length "++show (length row)++") was:\n  "++show row


