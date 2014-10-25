{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE NamedFieldPuns      #-}
-- |
-- Seeded from code by:
-- Copyright    : [2014] Trevor L. McDonell

module Main where

-- Friends:
import HSBencher
import HSBencher.Internal.Config (augmentResultWithConfig, getConfig)
import HSBencher.Backend.Fusion
import HSBencher.Backend.Fusion (defaultFusionPlugin)

import Criterion.Types                                  ( Report(..), SampleAnalysis(..), Regression(..) )
import Criterion.IO                                     ( readReports )
import Statistics.Resampling.Bootstrap                  ( Estimate(..), scale )

-- Standard:
import Control.Monad.Reader
import Data.Char
import Data.Text                                        ( Text )
import Data.Label
import Data.List as L
import Data.Monoid
import Control.Monad
import Network.HTTP                                     ( simpleHTTP, postRequestWithBody, Response(..) )
import Network.HTTP.Types                               ( urlEncode )
import System.IO
import System.Console.GetOpt (getOpt, getOpt', ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.Environment (getArgs)
import System.Exit
import qualified Data.ByteString.Char8                  as BS
import qualified Data.ByteString.Lazy.Char8             as BL
import qualified Data.Map                               as Map
import qualified Data.Text

import Debug.Trace

----------------------------------------------------------------------------------------------------

    

data Sample = Sample
  {
    sampleName          :: String       -- benchmark name
  , sampleTime          :: Estimate     -- sample result (via OLS regression)
  , sampleRSquare       :: Estimate     -- R^2 goodness-of-fit estimate of regression
  , sampleMean          :: Estimate     -- result mean
  , sampleStdDev        :: Estimate     -- standard deviation
  , sampleUnits         :: Unit
  }

data Unit = Unit
  {
    unitName            :: String       -- e.g. seconds
  , unitType            :: String       -- description of the unit, e.g. Time
  , lessIsMore          :: Bool         -- so python, much philosophical
  }


-- Treat a sample as being in milliseconds
--
ms :: Sample -> Sample
ms Sample{..} =
  Sample { sampleTime           = scale 1000 sampleTime
         , sampleRSquare        = scale 1000 sampleRSquare
         , sampleMean           = scale 1000 sampleMean
         , sampleStdDev         = scale 1000 sampleStdDev
         , sampleUnits          = Unit { unitName   = "ms"
                                       , unitType   = "Time"
                                       , lessIsMore = True }
         , ..
         }


-- Extract benchmark information from the Criterion report
--
-- TODO: Extract any other regressions present in the report into separate
--       samples, e.g. GC statistics
--
sample :: Report -> Sample
sample Report{..} = ms $ Sample
  { sampleName          = reportName
  , sampleTime          = regIters
  , sampleRSquare       = regRSquare
  , sampleMean          = anMean
  , sampleStdDev        = anStdDev
  , sampleUnits         = error "sample has no unit assigned"
  }
  where
    SampleAnalysis{..}  = reportAnalysis
    Regression{..}      = head anRegress
    regIters            = case Map.lookup "iters" regCoeffs of
                            Nothing -> error "sample: no benchmark result found"
                            Just t  -> t

_ = estPoint
    
{-
                                       
-- Encode a sample to a JSON value suitable for upload to codespeed
--
toValue :: Options -> Sample -> Value
toValue opt Sample{..} = object
  [ "commitid"          .= $(_HEAD)
  , "branch"            .= $(_BRANCH)
  , "project"           .= ("accelerate-examples" :: Text)
  , "executable"        .= get optVariant opt
  , "environment"       .= get optHostname opt
  , "benchmark"         .= sampleName
  , "result_value"      .= estPoint sampleTime
  , "min"               .= estLowerBound sampleTime
  , "max"               .= estUpperBound sampleTime
  , "std_dev"           .= estPoint sampleStdDev
  , "units"             .= unitName sampleUnits
  , "units_title"       .= unitType sampleUnits
  , "lessismore"        .= lessIsMore sampleUnits
  ]


-- Upload a set of criterion reports to the codespeed server
--
uploadReports :: Options -> [Report] -> IO ()
uploadReports opt reports =
  when upload $ do
    putStr ("Uploading results to " ++ server ++ " ... ") >> hFlush stdout
    rsp <- simpleHTTP $ postRequestWithBody url contentType content
    case rsp of
      Left err -> do
        putStrLn "failed"
        hPutStrLn stderr (show err)

      Right Response{..} ->
        case rspCode of
          (2,0,2) -> putStrLn "Ok"
          (a,b,c) -> do putStrLn $ map intToDigit [a,b,c] ++ ' ' : rspReason
                        hPutStrLn stderr rspBody
  where
    (upload, server)    = maybe (False, undefined) (True,) (get optCodespeed opt)

    contentType         = "application/x-www-form-urlencoded"
    url                 = server <> "/result/add/json/"
    payload             = "json=" <> encode (map (toValue opt . sample) reports)
    content             = BS.unpack . urlEncode False $ BL.toStrict payload
-}


-- main :: IO ()
-- main = putStrLn "Hello world"


data ExtraFlag = TableName String | PrintHelp
  deriving (Eq,Ord,Show,Read)

extra_cli_options :: [OptDescr ExtraFlag]
extra_cli_options =  [ Option ['h'] ["help"] (NoArg PrintHelp)
                       "Show this help message and exit."
                     , Option [] ["name"] (ReqArg TableName "NAME")
                       "Name for the fusion table to which we upload (discovered or created)." ]
plug :: FusionPlug
plug = defaultFusionPlugin

main :: IO ()
main = do
   cli_args <- getArgs
   let (help,fusion_cli_options) = plugCmdOpts plug

   let (opts1,plainargs,unrec,errs1) = getOpt' Permute extra_cli_options cli_args
   let (opts2,_,errs2) = getOpt Permute fusion_cli_options unrec
   let errs = errs1 ++ errs2   
   when (L.elem PrintHelp opts1 || not (null errs)) $ do 
     putStrLn $
       "USAGE: fusion-upload-criterion [options] REPORTFILE\n\n"++
       "Upload a pre-existing Criterion report benchmarked on the CURRENT machine.\n"++
       "This restriction is due to the Report not containing system information.  Rather,\n"++
       "'fusion-upload-criterion' gathers information about the platform at the time of upload.\n"++
       "\n"++
       (usageInfo "Options:" extra_cli_options)++"\n"++
       (usageInfo help fusion_cli_options)
     if null errs then exitSuccess else exitFailure

   let name = case [ n | TableName n <- opts1 ] of
               [] -> error "Must supply a table name!"
               [n] -> n
               ls  -> error $ "Multiple table names supplied!: "++show ls

   -- This bit could be abstracted nicely by the HSBencher lib:
   ------------------------------------------------------------
   -- Gather info about the benchmark platform:
   gconf0 <- getConfig [] []
   let gconf1 = gconf0 { benchsetName = Just name }
   gconf2 <- plugInitialize plug gconf1
   let fconf0 = getMyConf plug gconf2
   let fconf1 = foldFlags plug opts2 fconf0
   let gconf3 = setMyConf plug fconf1 gconf2
                
   ------------------------------------------------------------
   case plainargs of
     [] -> error "No file given to upload!"
     reports -> forM_ reports (doupload gconf3)

doupload :: Config -> FilePath -> IO ()
doupload confs file = do
  x <- readReports file
  case x of
    Left err -> error $ "Failed to read report file: \n"++err
    Right reports -> forM_ reports (upreport confs)

upreport :: Config -> Report -> IO ()
upreport gconf report = do
  let br  = emptyBenchmarkResult
  printReport report -- TEMP      
  br' <- augmentResultWithConfig gconf (addReport report br)
  runReaderT (uploadBenchResult br') gconf

printReport :: Report -> IO ()
printReport Report{..} = do 
  putStrLn ("Found report with keys: "++show reportKeys)
  let SampleAnalysis{..} = reportAnalysis
  forM_ anRegress $ \Regression{..} -> do
    putStrLn$ "  Regression: "++ show (regResponder, Map.keys regCoeffs)

addReport :: Report -> BenchmarkResult -> BenchmarkResult
addReport rep@Report{..} BenchmarkResult{..} =
  BenchmarkResult
  { _PROGNAME = reportName
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
