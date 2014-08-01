{-# LANGUAGE DeriveDataTypeable #-} 

module Main where

-- import Control.Monad.Reader
-- import qualified Data.Map as M
-- import Data.Time.Clock (getCurrentTime, diffUTCTime)
-- import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
-- import Data.Monoid
-- import Data.Dynamic
-- import GHC.Conc (getNumProcessors)
import System.Environment (getArgs, getEnv, getEnvironment)
import System.Console.GetOpt (getOpt', ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
-- import System.IO (Handle, hPutStrLn, stderr, openFile, hClose, hGetContents, hIsEOF, hGetLine,
--                   IOMode(..), BufferMode(..), hSetBuffering)
-- import qualified System.IO.Streams as Strm
-- import qualified System.IO.Streams.Concurrent as Strm
-- import qualified System.IO.Streams.Process as Strm
-- import qualified System.IO.Streams.Combinators as Strm
import Data.List (isInfixOf)
import Control.Monad (unless,when)
import System.Exit (exitFailure, exitSuccess)

-- import HSBencher.Types
-- import HSBencher.Internal.Utils
-- import HSBencher.Methods.Builtin
-- import HSBencher.Internal.MeasureProcess

import HSBencher.Internal.Fusion (init,getSomething,getWithSQLQuery,ColData,FTValue(..))

-- Exceptions
import Control.Exception
import Data.Typeable

-- SQL Parsing
import qualified Language.SQL.SimpleSQL.Parser as SQL
import qualified Language.SQL.SimpleSQL.Pretty as SQL
import qualified Language.SQL.SimpleSQL.Syntax as SQL


import qualified Prelude as P
import Prelude hiding (init) 
---------------------------------------------------------------------------
--



{- DEVLOG

   * ISSUES 

     Currently I get ResponseTimeout on all attempts to get
     the Dynaprof table. 
        - This happens when there is just too much data, use the query to limit what to pull

   * PLANS and Thoughts 

  
     - SQL Queries:
     Need to read up on what limitations the FT api put on these Queries.

     One thing I noticed is that the query takes the shape:
     SELECT xxxx FROM table_id WHERE yyy
     here table_id is some 'strange' unique identifier string that maybe the user does
     not really want to care about. 

     - Environment variables:
     Allow user to have secret and id as env variables.


   *Example usage

      -- QUERY 1
     /hsbencher do --secret=MQ72ZWDde_1e1ihI5YE9YlEi --id=925399326325-6dir7re3ik7686p6v3kkfkf1kj0ec7ck.apps.googleuserconteom --table=Dynaprof_Benchmarks --query="SELECT * FROM FT WHERE GIT_DEPTH = 445"

     -- QUERY 2 
     hsbencher do --secret=MQ72ZWDde_1e1ihI5YE9YlEi --id=925399326325-6dir7re3ik7686p6v3kkfkf1kj0ec7ck.apps.googleusercontent.com --table=Dynaprof_Benchmarks --query="SELECT 'MEDIANTIME' FROM FT WHERE GIT_DEPTH = 445 AND PROGNAME = 'h264ref-9.3'"

     -- QUERY 3
     hsbencher do --secret=MQ72ZWDde_1e1ihI5YE9YlEi --id=925399326325-6dir7re3ik7686p6v3kkfkf1kj0ec7ck.apps.googleusercontent.com --table=Dynaprof_Benchmarks --query="SELECT 'PROGNAME', 'VARIANT', 'MEDIANTIME', 'HOSTNAME' FROM FT WHERE GIT_DEPTH = 445 AND PROGNAME = 'h264ref-9.3'"
  

     The FROM field allows the text "FT" which is translated into the fusiontable id
     given we know the human readable name as passed into wiht --table=name

-} 




---------------------------------------------------------------------------
-- //                                                                 \\ --
---------------------------------------------------------------------------

-- | Command line flags to the benchmarking executable.
data Flag = ShowHelp | ShowVersion
          | GoogleSecret String | GoogleID String
          | FTName String
          | FTQuery String
-- CSV Convertion related Flags 
          | Wizard  -- Do your best
  deriving (Eq,Ord,Show,Read)

-- | Current run mode of the tool 
data Mode = Upload | Download
          deriving (Eq,Ord,Show,Read)

-- | Exceptions that may occur
data Error
  = FlagsNotValidE String
    deriving (Show, Typeable) 

instance Exception Error 


-- | List of valid operation modes of the hsbencher tool
valid_modes :: [String]
valid_modes = [ "upload", "download" ]

-- | Command line options.
core_cli_options :: [OptDescr Flag]
core_cli_options = 
     [ Option ['h'] ["help"] (NoArg ShowHelp)
        "Show this help message and exit."
     , Option []     ["secret"] (ReqArg GoogleSecret "String") "Google Secret"
     , Option []     ["id"]     (ReqArg GoogleID "String")     "Google ID"
     , Option []     ["table"]  (ReqArg FTName "String")       "Name of FusionTable"
     , Option ['q']  ["query"]  (ReqArg FTQuery "String")      "A SQL style query"
     , Option ['w']  ["Wizard"] (NoArg Wizard)                 "Generate a decent CSV file with no user guidance"
     ]

-- | Multiple lines of usage info help docs.
fullUsageInfo :: String
fullUsageInfo = usageInfo docs core_cli_options
 where 
  docs = "USAGE: hsbencher [mode] <flags> ...\n"++
         "\nValid modes: "++(unwords valid_modes)++
         "\n\nhsbencher-tool general options: \n"
--   ++ generalUsageStr

-- | Is a valid mode requested, if so turn it into a Mode.
--   a uniquely identifying infix of the mode is all that needs
--   to be recognized. So "up" and "do" are valid. 
resolveMode :: String -> Mode
resolveMode md = 
 case filter (isInfixOf md) valid_modes of
   ["download"] -> Download
   ["upload"]   -> Upload 
   []  -> error $ "Unknown mode for hsbencher tool: "++md
   ls  -> error $ "Ambiguous mode for hsbencher tool: "++md++", matches: "++unwords ls

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

  let (mode,rest) = 
        case plainargs of
         [] -> error $ "Command command expects the first argument to specify a mode, one of:\n" ++
                 unlines (map ("  "++) valid_modes) 
         (md:rst) -> (resolveMode md, rst)

  putStrLn ("hello world: "++show (mode,rest,options))

  ---------------------------------------------------------------------------
  -- Perform the task specified by the command line args

  catch (
    case mode of
      Download -> download options 
      Upload   -> upload options 
    ) (\e ->
        case e of
          FlagsNotValidE str -> putStrLn $ "Caught error: "++ str
      ) 



---------------------------------------------------------------------------
-- upload

upload :: [Flag] -> IO () 
upload = error "Upload functionality is not yet implemented"




---------------------------------------------------------------------------
-- download

download :: [Flag] -> IO ()
download flags = do 
  when (not flagsValid) $ throwIO $ FlagsNotValidE "The flags are invalid for a download"   


  putStrLn $ "processing table: "++ table
  putStrLn $ "Using ID: " ++ id
  putStrLn $ "Using Secret: " ++ secret

  ---------------------------------------------------------------------------
  -- Initialize !
  (table_id,auth) <- init id secret table


  ---------------------------------------------------------------------------
  -- is a query specified ?
  tab <- case hasQuery of 
    True -> 
      do
        let q = parseSQLQuery query
        case q of
          Left (SQL.ParseError msg _ _ fmsg) -> error $ msg ++ "\n" ++ fmsg
          Right validQuery -> do
            -- Replace "TABLE" with table_id in SQL Query
            let theQuery = metaID table_id validQuery
            -- Download whatever the query specifies
            pullWithQuery table_id auth (SQL.prettyQueryExpr theQuery)
    False -> 
      error "NO QUERY: Exiting"
       -- Here the tool should go into "simple mode" for users not
       -- in love with SQL. 

  putStrLn $ show tab
  where
    -- are flags valid for download ? 
    flagsValid =
      (not . null) [() | GoogleSecret _ <- flags] &&
      (not . null) [() | GoogleID _  <- flags] &&
      (not . null) [() | FTName _ <- flags] 

    -- did the user specify a query ? 
    hasQuery = (not . null) [ () | FTQuery _ <- flags]  
  
    -- assume flags valid
    secret = head [ c | GoogleSecret c <- flags]
    id     = head [ i | GoogleID i <- flags]
    table  = head [t | FTName t  <- flags]


    -- assume we have a query 
    query = head [ q | FTQuery q <- flags] 


---------------------------------------------------------------------------
-- Parse query
-- The purpose of this will (for now) simply be to check if the query is
-- wellformed. The hgdata FusionTable code currently takes the
-- query as a String (or is a ByteString).. 

parseSQLQuery :: String -> Either SQL.ParseError SQL.QueryExpr
parseSQLQuery str = SQL.parseQueryExpr "CommandLine" Nothing str 


---------------------------------------------------------------------------
-- As to not depend on the highly hacky Analytics.hs


--pullWithQuery :: String -> String -> String -> String -> IO ColData

--pullWithQuery cid sec table_name query = do
pullWithQuery table_id auth query = -- do
  -- putStrLn table_id
  getWithSQLQuery auth table_id query


---------------------------------------------------------------------------
-- metaID
-- Help the user, so he does not need to know the table_id

metaID :: String -> SQL.QueryExpr -> SQL.QueryExpr
metaID table_id qe@(SQL.Select _ _ _ _ _ _ _ _ _ ) =
  qe { SQL.qeFrom = map mangle (SQL.qeFrom qe) }
  where
    mangle (SQL.TRSimple names) = SQL.TRSimple $ map mangleNames names
    mangle sql = sql 
    
    mangleNames (SQL.Name str)   | str == "FT" = SQL.Name table_id
    mangleNames (SQL.QName str)  | str == "\"FT\"" = SQL.QName $ show table_id  
    mangleNames (SQL.UQName str) | str == "FT"  = SQL.UQName $show table_id  
    mangleNames n = n 

{-
  SQL queries look quite complicated to me.
  So here dealing with a subset of functionality.

  Need more SQL understanding, it might be possible to
  set an Alias. TRAlias. Thus change the Expr tree slightly at some
  higher location and have it permeate.
-} 






---------------------------------------------------------------------------
-- Pulled down table of FTValues to CSV

toCSV :: [[FTValue]] -> String
toCSV = undefined 




---------------------------------------------------------------------------
-- GUIDELINES FOR THE WIZARD
{-
    Identifying columns in the fusiontable will generally have StringValue fields. 
    Measurement points will generally be DoubleValue.

    



-} 
