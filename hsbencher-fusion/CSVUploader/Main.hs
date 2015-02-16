{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Network.Google.OAuth2 (OAuth2Client(..))
import Network.Google.FusionTables(CellType(..))

-- Standard:
import Control.Monad.Reader
import Data.List as L
import System.Console.GetOpt (getOpt, getOpt', ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.Environment (getArgs)
import System.Exit
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Version (showVersion)
import Paths_hsbencher_fusion (version)

import Text.CSV

this_progname :: String
this_progname = "hsbencher-fusion-upload-csv"

----------------------------------------------------------------------------------------------------

data ExtraFlag = TableName String
               | PrintHelp
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
       this_progname++": "++showVersion  version++"\n"++
       "USAGE: "++this_progname++" [options] CSVFILE\n\n"++
       "Upload pre-existing CSV, e.g. data as gathered by the 'dribble' plugin.\n"++
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
   let fconf0 = getMyConf plug gconf1
   let fconf1 = foldFlags plug opts2 fconf0
   let gconf2 = setMyConf plug fconf1 gconf1       
   gconf3 <- plugInitialize plug gconf2                

   ------------------------------------------------------------
   case plainargs of
     [] -> error "No file given to upload!"
     reports -> forM_ reports (doupload gconf3)

doupload :: Config -> FilePath -> IO ()
doupload confs file = do
  x <- parseCSVFromFile file  
  case x of
    Left err -> error $ "Failed to read CSV file: \n"++show err
    Right [] -> error $ "Bad CSV file, not even a header line: "++file
    Right (hdr:rst) -> do
      checkHeader hdr
      putStrLn$ " ["++this_progname++"] Beginning upload CSV data with Schema: "++show hdr
      serverSchema <- ensureMyColumns confs hdr -- FIXUP server schema.
      putStrLn$ " ["++this_progname++"] Uploading "++show (length rst)++" rows of CSV data..."
      putStrLn "================================================================================"
      uprows confs serverSchema hdr rst
      

-- TODO: Add checking to see if the rows are already there.  However
-- that would be expensive if we do one query per row.  The ideal
-- implementation would examine the structure of the rowset and make
-- fewer queries.

-- | Perform the actual upload of N rows
uprows :: Config -> Schema -> Schema -> [[String]] -> IO ()
uprows confs serverSchema hdr rst = do
      let missing = S.difference (S.fromList serverSchema) (S.fromList hdr)  
      -- Compute a base benchResult to fill in our missing fields:
      base <- if S.null missing then
                return emptyBenchmarkResult -- Don't bother computing it, nothing missing.
              else do
                putStrLn $ "\n\n ["++this_progname++"] Fields missing, filling in defaults: "++show (S.toList missing)
                -- Start with the empty tuple, augmented with environmental info:
                augmentResultWithConfig confs emptyBenchmarkResult

      let tuples = map (zip hdr) rst
          prepped = map (prepBenchResult serverSchema) $
                    map (`unionBR` base) tuples
      putStrLn$ " ["++this_progname++"] Tuples prepped.  Here's the first one: "++ show (head prepped)
      -- Layer on what we have.
      runReaderT (uploadRows prepped) confs

-- | Union a tuple with a BenchmarkResult.  Any unmentioned keys in
-- the tuple retain their value from the input BenchmarkResult.
unionBR :: [(String,String)] -> BenchmarkResult -> BenchmarkResult
unionBR tuple br1 =
   tupleToResult (M.toList (M.union (M.fromList tuple)
                            (M.fromList (resultToTuple br1))))


-- FIXUP: FusionConfig doesn't document our additional CUSTOM columns.
-- During initialization it ensures the table has the core schema, but that's it.
-- Thus we need to make sure ALL columns are present.
ensureMyColumns  :: Config -> [String] -> IO Schema
ensureMyColumns confs hdr = do
      let FusionConfig{fusionTableID,fusionClientID,fusionClientSecret,serverColumns} = getMyConf plug confs
          (Just tid, Just cid, Just sec) = (fusionTableID, fusionClientID, fusionClientSecret)
          auth = OAuth2Client { clientId=cid, clientSecret=sec }
          missing = S.difference (S.fromList hdr) (S.fromList serverColumns)
          -- HACK: we pretend everything in a STRING here... we should probably look at the data in the CSV
          -- and guess if its a number.  However, if columns already exist we DONT change their type, so it
          -- can always be done manually on the server.
          schema = [ case lookup nm fusionSchema of
                       Nothing -> (nm,STRING)
                       Just t  -> (nm,t)
                   | nm <- serverColumns ++ S.toList missing ]
      if S.null missing
        then putStrLn$ " ["++this_progname++"] Server has all the columns appearing in the CSV file.  Good."
        else putStrLn$ " ["++this_progname++"] Adding missing columns: "++show missing
      res <- runReaderT (ensureColumns auth tid schema) confs
      putStrLn$ " ["++this_progname++"] Done adding, final server schema:"++show res
      return res


checkHeader :: Record -> IO ()
checkHeader hdr
  | L.elem "PROGNAME" hdr = return ()
  | otherwise = error $ "Bad HEADER line on CSV file.  Expecting at least PROGNAME to be present: "++show hdr

