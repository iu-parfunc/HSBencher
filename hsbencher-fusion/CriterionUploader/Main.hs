{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
-- |
-- Seeded from code by:
-- Copyright    : [2014] Trevor L. McDonell

module Main where

-- Friends:
import HSBencher
import HSBencher.Internal.Config (augmentResultWithConfig, getConfig)
import HSBencher.Backend.Fusion
import HSBencher.Backend.Dribble (defaultDribblePlugin, DribbleConf (..))

import Criterion.Types                                  ( Report(..), SampleAnalysis(..), Regression(..) )
import Criterion.IO                                     ( readReports )
import Statistics.Resampling.Bootstrap                  ( Estimate(..) )

-- Standard:
import Control.Monad.Reader
import Data.List as L
import Data.Char (isSpace)
import System.Console.GetOpt (getOpt, getOpt', ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.Environment (getArgs, getProgName)
import System.Exit
import qualified Data.Map                               as Map

import Data.Version (showVersion)
import Paths_hsbencher_fusion (version)

----------------------------------------------------------------------------------------------------

data ExtraFlag = TableName String
               | SetVariant   String
               | SetArgs      String
               | SetThreads   Int
                 -- TODO: this should include MOST of the schema's
                 -- fields... we need a scalable way to do this.
                 -- Applicative options would help...
--               | SetHostname  String 

               | WriteCSV     FilePath
               | NoUpload
               | PrintHelp
  deriving (Eq,Ord,Show,Read)

extra_cli_options :: [OptDescr ExtraFlag]
extra_cli_options =  [ Option ['h'] ["help"] (NoArg PrintHelp)
                       "Show this help message and exit."
                     , Option [] ["name"] (ReqArg TableName "NAME")
                       "Name for the fusion table to which we upload (discovered or created)."
                     , Option [] ["variant"] (ReqArg SetVariant "STR")
                       "Setting for the VARIANT field for *ALL* uploaded data from the given report."
                     , Option [] ["args"] (ReqArg SetArgs "STR")
                       "Set the ARGS column in the uploaded data."
                     , Option [] ["threads"] (ReqArg (SetThreads . safeRead) "NUM")
                       "Set the THREADS column in the uploaded data."

                     , Option [] ["csv"] (ReqArg WriteCSV "PATH")
                       "Write the Criterion report data into a CSV file using the HSBencher schema."
                     , Option [] ["noupload"] (NoArg NoUpload)
                       "Don't actually upload to the fusion table (but still psosible write CSV)."
                     ]

safeRead :: String -> Int
safeRead x = case reads (trim x) of
              (n,[]):_ -> n
              _        -> error $ "error: could not parse as Int: "++x

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

plug :: FusionPlug
plug = defaultFusionPlugin

main :: IO ()
main = do
   cli_args <- getArgs
   let (help,fusion_cli_options) = plugCmdOpts plug

   let (opts1,plainargs,unrec,errs1) = getOpt' Permute extra_cli_options cli_args
   let (opts2,_,errs2) = getOpt Permute fusion_cli_options unrec
   let errs = errs1 ++ errs2
   progName <- getProgName
   when (L.elem PrintHelp opts1 || not (null errs)) $ do 
     putStrLn $
       "USAGE: "++progName++" [options] REPORTFILE\n"++
       "Version: "++ showVersion version++"\n\n"++       
       
       "Upload a pre-existing Criterion report benchmarked on the CURRENT machine.\n"++
       "This restriction is due to the Report not containing system information.  Rather,\n"++
       "'fusion-upload-criterion' gathers information about the platform at the time of upload.\n"++
       "\n"++
       (usageInfo "Options:" extra_cli_options)++"\n"++
       (usageInfo help fusion_cli_options)
     if null errs then exitSuccess else exitFailure

   let noup = L.elem NoUpload opts1
   let csvPath = (\(WriteCSV path) -> path) `fmap`
                     L.find (\case WriteCSV _ -> True
                                   _ -> False) opts1

   let name = case [ n | TableName n <- opts1 ] of
               [] -> error "Must supply a table name!"
               [n] -> n
               ls  -> error $ "Multiple table names supplied!: "++show ls

   let presets1 = emptyBenchmarkResult
   let presets2 = case [ n | SetVariant n <- opts1 ] of
                  []  -> presets1
                  [n] -> presets1 { _VARIANT = n }
                  ls  -> error $ "Multiple VARIANTs supplied!: "++show ls
   let presets3 = case [ n | SetArgs n <- opts1 ] of
                  []  -> presets2
                  [n] -> presets2 { _ARGS = words n }
                  ls  -> error $ "Multiple ARGS settings supplied!: "++show ls
       presets4 = case [ n | SetThreads n <- opts1 ] of
                  []  -> presets3
                  [n] -> presets3 { _THREADS = n }
                  ls  -> error $ "Multiple THREADS settings supplied!: "++show ls
   
   -- This bit could be abstracted nicely by the HSBencher lib:
   ------------------------------------------------------------
   -- Gather info about the benchmark platform:
   gconf0 <- getConfig [] []
   let gconf1 = gconf0 { benchsetName = Just name }
   let fconf0 = getMyConf plug gconf1
   let fconf1 = foldFlags plug opts2 fconf0
   let gconf2 = setMyConf plug fconf1 gconf1       
   gconf3 <- if noup then return gconf2 else plugInitialize plug gconf2

   ------------------------------------------------------------
   case plainargs of
     [] -> error "No file given to upload!"
     reports -> do
       maybe (return ()) (doCSV gconf3 presets4 reports) csvPath
       unless noup $ forM_ reports (doupload gconf3 presets4)

doupload :: Config -> BenchmarkResult -> FilePath -> IO ()
doupload confs presets file = do
  x <- readReports file
  case x of
    Left err -> error $ "Failed to read report file: \n"++err
    Right reports -> forM_ reports (upreport confs presets)

doCSV :: Config -> BenchmarkResult -> [FilePath] -> FilePath -> IO ()
doCSV confs presets reportFiles csvFile = do
  brs <- concat `fmap` forM reportFiles (\reportFile -> do
           critReport <- readReports reportFile
           case critReport of
             Left err -> error $ "Failed to read report file " ++ reportFile ++ ": \n" ++ err
             Right reports -> mapM (augmentResultWithConfig confs . flip addReport presets) reports)
  -- TODO: need to change file names here
  forM_ brs $ \benchRet -> do
    -- we restart dribble plugin to set a new path for each report
    let updateDribbleConf =
          Map.insert "dribble" (SomePluginConf defaultDribblePlugin $ DribbleConf (Just csvFile))
    dribbleConf <- plugInitialize defaultDribblePlugin
                     confs{plugInConfs=updateDribbleConf (plugInConfs confs)}
    void $ plugUploadRow defaultDribblePlugin dribbleConf benchRet

upreport :: Config -> BenchmarkResult -> Report -> IO ()
upreport gconf presets report = do
  printReport report -- TEMP
  br' <- augmentResultWithConfig gconf (addReport report presets)
  runReaderT (uploadBenchResult br') gconf

printReport :: Report -> IO ()
printReport Report{..} = do 
  putStrLn ("Found report with keys: "++show reportKeys)
  let SampleAnalysis{..} = reportAnalysis
  forM_ anRegress $ \Regression{..} -> do
    putStrLn$ "  Regression: "++ show (regResponder, Map.keys regCoeffs)

addReport :: Report -> BenchmarkResult -> BenchmarkResult
addReport Report{..} BenchmarkResult{..} =
  BenchmarkResult
  { _PROGNAME = reportName
  , _VARIANT = if null _VARIANT
               then "criterion" -- This is just helpful for filtering down the fusion table.
               else _VARIANT
  , _MEDIANTIME = medtime
  , _MINTIME    = estLowerBound $ fetch "time" "iters"
  , _MAXTIME    = estUpperBound $ fetch "time" "iters"

  , _MEDIANTIME_PRODUCTIVITY =
    do ms <- Map.lookup ("mutatorWallSeconds","iters") ests
       gs <- Map.lookup ("gcWallSeconds","iters") ests       
       return (estPoint ms / (estPoint ms + estPoint gs))
    
  , _MEDIANTIME_ALLOCRATE =
    do e <- Map.lookup ("allocated","iters") ests
       -- Use time to extrapolate the alloc rate / second:
       return (round(estPoint e * (1.0 / medtime)))

  , _CUSTOM =
    (maybe [] (\ e -> [("BYTES_ALLOC",DoubleResult (estPoint e))])
              (Map.lookup ("allocated","iters") ests)) ++

    (maybe [] (\ e -> [("BYTES_COPIED",DoubleResult (estPoint e))])
              (Map.lookup ("bytesCopied","iters") ests)) ++ 

    (maybe [] (\ e -> [("NUMGC",DoubleResult (estPoint e))])
              (Map.lookup ("numGcs","iters") ests)) ++ 

    (maybe [] (\ e -> [("CPUTIME",DoubleResult (estPoint e))])
              (Map.lookup ("cpuTime","iters") ests)) ++ 
        
    (maybe [] (\ e -> [("CYCLES",DoubleResult (estPoint e))])
              (Map.lookup ("cycles","iters") ests))
  , ..
  }
  where
    medtime = estPoint $ fetch "time" "iters"
    
    SampleAnalysis{..} = reportAnalysis
    ests = Map.fromList $
             [ ((regResponder,p), e) | Regression{..} <- anRegress
                                     , (p,e) <- Map.toList regCoeffs ]
    fetch r p = case Map.lookup (r,p) ests of
                  Nothing -> error $ "Expected regression with responder/predictor:"++r++"/"++p
                  Just x  -> x
