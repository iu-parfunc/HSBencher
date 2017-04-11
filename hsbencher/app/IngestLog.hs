{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simple application to parse logs.

module Main where

import           Control.Monad.Reader
import           Options.Applicative
import           System.FilePath
import           Data.Maybe
import           Data.Default 
import           Data.List.Split (splitWhen)
-- import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BS
import           Prelude as P hiding (log)
import           Text.Show.Pretty (ppShow)
   
import           HSBencher.Harvesters
import           HSBencher.Backend.Dribble
import           HSBencher.Internal.Config
import           HSBencher.Internal.App
import           HSBencher.Internal.Logging (log)
import           HSBencher.Types
--------------------------------------------------------------------------------    

data Opts = Opts { infile  :: Maybe FilePath
                 , outfile :: Maybe FilePath
                 }
 deriving (Eq,Show,Read,Ord)
          
argsParser :: Parser Opts
argsParser = Opts 
             <$> optional (strArgument (metavar "InputFile.log"))
             <*> optional (strArgument (metavar "OutputFile.csv"))

-- https://github.com/iu-parfunc/HSBencher/issues/75
    
main :: IO ()
main = do opts <- execParser $ info (helper <*> argsParser)
                  ( fullDesc
                  <> progDesc (P.unlines
                               [ "Utility to ingest logs and produce HSBencher-formatted CSV output."
                               , "As described in: https://github.com/iu-parfunc/HSBencher/issues/75"
                               , "Writes to the specified input/output files, if present, or from/to stdin/stdout otherwise."
                               ])
                  <> header "hsbencher-ingest-log" )
          ingestLog opts

-- TODO: Should do this all with constant space, conduit, etc...
ingestLog :: Opts -> IO ()
ingestLog opts@Opts{infile,outfile} = do
  putStrLn $ "Ingesting logs  "++show opts
  input <- case infile of
             Just f  -> BS.readFile f
             Nothing -> BS.getContents 
  cfg0 <- getConfig [] []
  let cfg1 = addPlugin defaultDribblePlugin (DribbleConf { csvfile = outfile }) cfg0          
  cfg2@Config{ harvesters } <- plugInitialize defaultDribblePlugin cfg1

  -- ADD harvesters for PROGNAME, ARGS, VARIANT
  let harvs = taggedRawHarvester "PROGNAME" (\x r -> r{ _PROGNAME = x }) <>
              taggedRawHarvester "VARIANT"  (\x r -> r{ _VARIANT  = x }) <>
              -- taggedLineHarvester "ARGS"     (\x r -> r{ _ARGS     = words x }) <>
              harvesters

  let runRess = log2Runs input
  let results = parseRun def harvs (RunCompleted 0.0 (map OutLine (BS.lines input)))
              
  putStrLn $ "Config: "++ppShow (cfg2)

  putStrLn $ "Extracted "++show (length runRess)++" individual benchmark results."
  putStrLn $ "Parsed "++show (length results)++" individual benchmark results."
           
-- augmentBenchmarkResult
-- parseRun

  runReaderT (do printBenchrunHeader
                 log "Printing output to files..."
                 mapM_ runC_outputBenchmarkResult results
                 log "Done."
             )
     cfg2

log2Runs :: BS.ByteString -> [RunResult]
log2Runs bs0 = [ RunCompleted 0.0 (map OutLine lns) | lns <- trimmed ]
  where
    allLines = BS.lines bs0
    chunks = case splitWhen isStart allLines of
               []   -> error "internal error"
               [[]] -> error "hsbencher-ingest-log: log did not contain START_BENCHMARK tag."
               (_trash:rest) -> rest -- Throw away the bit up to the FIRST START_BENCHMARK
    trimmed = [ takeWhile (not . isEnd) c | c <- chunks]



isStart :: BS.ByteString -> Bool
isStart b =
 case BS.words b of
   ("START_BENCHMARK":_)  -> True
   ("START_BENCHMARK:":_) -> True
   _                      -> False


isEnd :: BS.ByteString -> Bool
isEnd b =
 case BS.words b of
   ("END_BENCHMARK":_)  -> True
   ("END_BENCHMARK:":_) -> True
   _                    -> False
