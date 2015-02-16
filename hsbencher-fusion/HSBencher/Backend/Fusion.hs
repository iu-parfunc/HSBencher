{-# LANGUAGE NamedFieldPuns, RecordWildCards, ScopedTypeVariables, CPP, BangPatterns #-}
{-# LANGUAGE TupleSections, DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

-- | Google Fusion Table upload of benchmark data.
-- 
--   This module must be used in conjunction with the main "hsbencher" package,
--   e.g. "import HSBencher".

module HSBencher.Backend.Fusion
       ( -- * The plugin itself, what you probably want 
         defaultFusionPlugin

         -- * Creating and finding tables
       , getTableId, findTableId, makeTable, ensureColumns
         
         -- * Details and configuration options.
       , FusionConfig(..), stdRetry
       , fusionSchema, resultToTuple

         -- * Prepping and uploading tuples (rows)
       , PreppedTuple, Schema
       , authenticate, prepBenchResult, uploadRows
       , uploadBenchResult

       , FusionPlug(), FusionCmdLnFlag(..),
       )
       where

import Control.Monad.Reader
import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Data.Maybe (fromJust, fromMaybe)
import Data.Dynamic
import Data.Default (Default(..))
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import Data.Time.Clock
import Data.Time.Format ()
import Network.Google.OAuth2 (getCachedTokens, refreshTokens, OAuth2Client(..), OAuth2Tokens(..))
import Network.Google.FusionTables (createTable, createColumn, listTables, listColumns,
                                    bulkImportRows, 
                                    TableId, CellType(..), TableMetadata(..), ColumnMetadata(..))
import Network.HTTP.Conduit (HttpException)
import HSBencher.Types
import HSBencher.Internal.Logging (log)
import HSBencher.Internal.Fusion
import Prelude hiding (log)
import System.Console.GetOpt (OptDescr(Option), ArgDescr(..))

-- | A default plugin.  This binding provides future-proof way to get
--   a default instance of the plugin, in the eventuality that more
--   configuration options are added in the future.
defaultFusionPlugin :: FusionPlug
defaultFusionPlugin = FusionPlug

-- | This is the same as defaultFusionPlugin
instance Default FusionPlug where
  def = defaultFusionPlugin

----------------------------------------------------------------------------------------------------
-- #ifdef FUSION_TABLES
-- import Network.Google.OAuth2 (getCachedTokens, refreshTokens, OAuth2Client(..), OAuth2Tokens(..))
-- import Network.Google.FusionTables (createTable, listTables, listColumns, insertRows,
--                                     TableId, CellType(..), TableMetadata(..))
-- import HSBencher.Fusion (getTableId, fusionPlugin)
-- #endif

----------------------------------------------------------------------------------------------------

-- defaultColumns =
--   ["Program","Args","Threads","Sched","Threads",
--    "MinTime","MedianTime","MaxTime", "MinTime_Prod","MedianTime_Prod","MaxTime_Prod"]


-- | The standard retry behavior when receiving HTTP network errors.  Note that this
-- can retry for quite a long while so it is only to be usedfrom batch applications.
stdRetry :: String -> OAuth2Client -> OAuth2Tokens -> IO a ->
            BenchM (Maybe a)
stdRetry msg client toks action = do
  conf <- ask
  let retryHook num exn = runReaderT (do
        datetime <- lift$ getDateTime
        log$ " [fusiontable] Retry #"++show num++" during <"++msg++"> due to HTTPException: " ++ show exn
        log$ " [fusiontable] ("++datetime++") Retrying, but first, attempt token refresh..."
        -- QUESTION: should we retry the refresh itself, it is NOT inside the exception handler.
        -- liftIO$ refreshTokens client toks
        -- liftIO$ retryIORequest (refreshTokens client toks) (\_ -> return ()) [1,1]
        stdRetry "refresh tokens" client toks (refreshTokens client toks)
        return ()
                                     ) conf
  liftIO$ retryIORequest action retryHook $
          [1,2,4,4,4,4,4,4,8,16] --- 32,64,
          ++ replicate 30 5

getDateTime :: IO String
getDateTime = do 
  utc <- getCurrentTime
  -- let day  = utctDay utc
  --     secs = utctDayTime utc
--  return $ show day ++" "++show secs
  return $ show utc

-- | Takes an idempotent IO action that includes a network request.  Catches
-- `HttpException`s and tries a gain a certain number of times.  The second argument
-- is a callback to invoke every time a retry occurs.
-- 
-- Takes a list of *seconds* to wait between retries.  A null list means no retries,
-- an infinite list will retry indefinitely.  The user can choose whatever temporal
-- pattern they desire (e.g. exponential backoff).
--
-- Once the retry list runs out, if it has not been successful, this function returns Nothing.
retryIORequest :: IO a -> (Int -> HttpException -> IO ()) -> [Double] -> IO (Maybe a)
retryIORequest req retryHook times = loop 0 times
  where
    loop _ [] = return Nothing
    loop !num (delay:tl) = 
      E.catch (fmap Just req) $ \ (exn::HttpException) -> do 
        retryHook num exn
        threadDelay (round$ delay * 1000 * 1000) -- Microseconds
        loop (num+1) tl

fromJustErr :: String -> Maybe t -> t
fromJustErr msg Nothing  = error msg
fromJustErr _   (Just x) = x


-- TODO: Should probably move these routines into some kind of
-- "Authenticated" monad which would provide a Reader for the auth
-- info.

-- | Get the table ID that has been cached on disk, or find the the table in the users
-- Google Drive, or create a new table if needed.
--
-- This is a simple shorthand for combining findTableId/makeTable/ensureColumns.  
--
-- It adds columns if necessary and returns the permutation of columns
-- found server side.  It assumes the DEFAULT core table Schema and
-- will not work for creating CUSTOM columns on the server side.
-- Simple drop down to using the three finer grained routines if you want that.
getTableId :: OAuth2Client -> String -> BenchM (TableId, [String])
getTableId auth tablename = do
  x <- findTableId auth tablename
  tid <- case x of
           Nothing -> makeTable auth tablename
           Just iD -> return iD
  order <- ensureColumns auth tid fusionSchema
  return (tid, order)

-- | Look for a table by name, returning its ID if it is present.
findTableId :: OAuth2Client -> String -> BenchM (Maybe TableId)
findTableId auth tablename = do
  log$ " [fusiontable] Fetching access tokens, client ID/secret: "++show (clientId auth, clientSecret auth)
  toks      <- liftIO$ getCachedTokens auth
  log$ " [fusiontable] Retrieved: "++show toks
  let atok  = B.pack $ accessToken toks
  allTables <- fmap (fromJustErr "[fusiontable] getTableId, API call to listTables failed.") $
               stdRetry "listTables" auth toks $ listTables atok
  log$ " [fusiontable] Retrieved metadata on "++show (length allTables)++" tables"
  case filter (\ t -> tab_name t == tablename) allTables of
    [] -> do log$ " [fusiontable] No table with name "++show tablename 
             return Nothing
    [t] -> do let tid = (tab_tableId t)
              log$ " [fusiontable] Found one table with name "++show tablename ++", ID: "++show tid
              return (Just tid)

-- | Make a new table, returning its ID.
--   In the future this may provide failure recovery.  But for now it
--   simply produces an exception if anything goes wrong.
--   And in particular there is no way to deal with multiple clients
--   racing to perform a `makeTable` with the same name.
makeTable :: OAuth2Client -> String -> BenchM TableId
makeTable auth tablename = do
  toks      <- liftIO$ getCachedTokens auth
  let atok  = B.pack $ accessToken toks  
  log$ " [fusiontable] No table with name "++show tablename ++" found, creating..."
  Just TableMetadata{tab_tableId} <- stdRetry "createTable" auth toks $
                                     createTable atok tablename fusionSchema
  log$ " [fusiontable] Table created with ID "++show tab_tableId
  -- TODO: IF it exists but doesn't have all the columns, then add the necessary columns.
  return tab_tableId

-- | Make sure that a minimal set of columns are present.  This
--   routine creates columns that are missing and returns the
--   permutation of columns found on the server.              
ensureColumns :: OAuth2Client -> TableId -> [(String, CellType)] -> BenchM [String]
ensureColumns auth tid ourSchema = do
  log$ " [fusiontable] ensureColumns: Ensuring schema: "++show ourSchema
  toks      <- liftIO$ getCachedTokens auth
  log$ " [fusiontable] ensureColumns: Retrieved: "++show toks
  let ourColNames = map fst ourSchema
  let atok      = B.pack $ accessToken toks
  let ourSet    = S.fromList ourColNames
  log$ " [fusiontable] ensureColumns: Checking columns... "  
  targetColNames <- fmap (map col_name) $ liftIO$ listColumns atok tid
  let targetSet = S.fromList targetColNames
      missing   = S.difference ourSet targetSet
      misslist  = L.filter (`S.member` missing) ourColNames -- Keep the order.
      extra     = S.difference targetSet ourSet
  unless (targetColNames == ourColNames) $ 
    log$ "WARNING: HSBencher upload schema (1) did not match server side schema (2):\n (1) "++
         show ourSchema ++"\n (2) " ++ show targetColNames
         ++ "\n HSBencher will try to make do..."
  unless (S.null missing) $ do                
    log$ "WARNING: These fields are missing server-side, creating them: "++show misslist
    forM_ misslist $ \ colname -> do
      Just ColumnMetadata{col_name, col_columnId} <- stdRetry "createColumn" auth toks $
                                                     createColumn atok tid (colname, STRING)
      log$ "   -> Created column with name,id: "++show (col_name, col_columnId)
  unless (S.null extra) $ do
    log$ "WARNING: The fusion table has extra fields that HSBencher does not know about: "++
         show (S.toList extra)
    log$ "         Expect null-string entries in these fields!  "
  -- For now we ASSUME that new columns are added to the end:
  -- TODO: We could do another read from the list of columns to confirm.
  return (targetColNames ++ misslist)

  
{-# DEPRECATED uploadBenchResult "this is subsumed by the Plugin interface and uploadRows" #-}
-- | Push the results from a single benchmark to the server.
uploadBenchResult :: BenchmarkResult -> BenchM ()
uploadBenchResult  br = do
  (_toks,auth,tid) <- authenticate  
  order <- ensureColumns auth tid fusionSchema  
  let row = prepBenchResult order br      
  flg <- uploadRows [row]
  unless flg $ error "uploadBenchResult: failed to upload rows"

-- | Upload raw tuples that are already in the format expected on the server.
--   Returns True if the upload succeeded.
uploadRows :: [PreppedTuple] -> BenchM Bool
uploadRows rows = do
  (toks,auth,tid) <- authenticate
  ---------------------  
  let colss = map (map fst) rows
      dats  = map (map snd) rows 
  case colss of
    [] -> return True
    (schema:rst) -> do
       unless (all (== schema) rst) $
         error ("uploadRows: not all Schemas matched: "++ show (schema, filter (/= schema) rst))
       -- It's easy to blow the URL size; we need the bulk import version.
       -- stdRetry "insertRows" authclient toks $ insertRows
       res <- stdRetry "bulkImportRows" auth toks $ bulkImportRows
               (B.pack$ accessToken toks) tid schema dats
       case res of 
         Just _  -> do log$ " [fusiontable] Done uploading, run ID "++ (fromJust$ lookup "RUNID" (head rows))
                         ++ " date "++ (fromJust$ lookup "DATETIME" (head rows))
                       return True
         Nothing -> do log$ " [fusiontable] WARNING: Upload failed the maximum number of times.  Continuing with benchmarks anyway"
                       return False

-- | Check cached tokens, authenticate with server if necessary, and
-- return a bundle of the commonly needed information to speak to the
-- Fusion Table API.
authenticate :: BenchM (OAuth2Tokens, OAuth2Client, TableId)
authenticate = do
  conf <- ask
  let fusionConfig = getMyConf FusionPlug conf
 --    fusionConfig <- error "FINISHME - acquire config dynamically"
  let FusionConfig{fusionClientID, fusionClientSecret, fusionTableID, serverColumns} = fusionConfig
  let (Just cid, Just sec) = (fusionClientID, fusionClientSecret)
      auth = OAuth2Client { clientId = cid, clientSecret = sec }  
  toks  <- liftIO$ getCachedTokens auth
  let atok  = B.pack $ accessToken toks
  let tid = fromJust fusionTableID 
  return (toks,auth,tid)

-- | Tuples in the format expected on the server.
type PreppedTuple = [(String,String)]

-- | The ordered set of column names that form a schema.
type Schema = [String] -- TODO: include types.

{-
-- | Ensure that a Schema is available on the server, creating columns
-- if necessary.  
ensureSchema :: Schema -> BenchM ()
ensureSchema ourSchema = do
    let (Just cid, Just sec) = (fusionClientID, fusionClientSecret)
        authclient = OAuth2Client { clientId = cid, clientSecret = sec }  
    -- FIXME: it's EXTREMELY inefficient to authenticate on every tuple upload:
    toks  <- liftIO$ getCachedTokens authclient
    let atok  = B.pack $ accessToken toks
    let tid = fromJust fusionTableID 
  
-- ////// Enable working with Custom tags
    let -- ourSchema = map fst $ benchmarkResultToSchema br
        ourSet    = S.fromList ourSchema 
    if null _CUSTOM 
     then log$ " [fusiontable] Computed schema, no custom fields."
     else log$ " [fusiontable] Computed schema, including these custom fields: " ++ show _CUSTOM
    targetSchema <- fmap (map col_name) $ liftIO$ listColumns atok tid      
    let targetSet = S.fromList targetSchema
        missing   = S.difference  ourSet targetSet
        misslist  = L.filter (`S.member` missing) ourSchema
    log$ " [fusiontable] There were " ++ show (length misslist) ++ " columns missing"
    unless (S.null missing) $ do
      forM_ misslist $ \ colname -> do
        stdRetry "createColumn" authclient toks $
                createColumn atok tid (colname, STRING)
        -- Create with the correct type !? Above just states STRING.          
--- ////// END
-}

  
-- | Prepare a Benchmark result for upload, matching the given Schema
-- in order and contents, which may mean adding empty fields.
-- This function requires that the Schema already contain all columns
-- in the benchmark result.
prepBenchResult :: Schema -> BenchmarkResult -> PreppedTuple
prepBenchResult serverColumns br@BenchmarkResult{..} = 
{-  
    conf <- ask
    let fusionConfig = getMyConf FusionPlug conf
--    fusionConfig <- error "FINISHME - acquire config dynamically"
    let FusionConfig{fusionClientID, fusionClientSecret, fusionTableID, serverColumns} = fusionConfig
    let (Just cid, Just sec) = (fusionClientID, fusionClientSecret)
        authclient = OAuth2Client { clientId = cid, clientSecret = sec }

    
    -- FIXME: it's EXTREMELY inefficient to authenticate on every tuple upload:
    toks  <- liftIO$ getCachedTokens authclient
    let atok  = B.pack $ accessToken toks
    let tid = fromJust fusionTableID 

-}
    let 
        ourData   = M.fromList $ resultToTuple br
        ourCols   = M.keysSet ourData
        targetSet = S.fromList serverColumns
        missing   = S.difference ourCols targetSet
        -- Any field HSBencher doesn't know about just gets an empty string:
        tuple   = [ (key, fromMaybe "" (M.lookup key ourData))
                  | key <- serverColumns ]
    in if S.null missing 
       then tuple
       else error $ "prepBenchResult: benchmark result contained columns absent on server: "++show missing
{-        
    log$ " [fusiontable] Uploading row with "++show (length cols)++
         " columns containing "++show (sum$ map length vals)++" characters of data"
    log$ " [fusiontable] Full row contents: "++show ourData
    return tuple
-}



-- | A representaton used for creating tables.  Must be isomorphic to
-- `BenchmarkResult`.  This could perhaps be generated automatically
-- (e.g. from a Generic instance, or even by creating a default
-- benchmarkResult and feeding it to resultToTuple).
-- 
-- Note, order is important here, because this is the preferred order we'd like to
-- have it in the Fusion table.
--  
fusionSchema :: [(String, CellType)]
fusionSchema =
  [ ("PROGNAME",STRING)
  , ("VARIANT",STRING)
  , ("ARGS",STRING)    
  , ("HOSTNAME",STRING)
  , ("MINTIME", NUMBER)
  , ("MEDIANTIME", NUMBER)
  , ("MAXTIME", NUMBER)
  , ("THREADS",NUMBER)
  , ("RETRIES",NUMBER)
  -- The run is identified by hostname_secondsSinceEpoch:
  , ("RUNID",STRING)
  , ("CI_BUILD_ID",STRING)  
  , ("DATETIME",DATETIME)    
  , ("MINTIME_PRODUCTIVITY", NUMBER)
  , ("MEDIANTIME_PRODUCTIVITY", NUMBER)
  , ("MAXTIME_PRODUCTIVITY", NUMBER)
  , ("ALLTIMES", STRING)
  , ("TRIALS", NUMBER)
  , ("COMPILER",STRING)
  , ("COMPILE_FLAGS",STRING)
  , ("RUNTIME_FLAGS",STRING)
  , ("ENV_VARS",STRING)
  , ("BENCH_VERSION", STRING)
  , ("BENCH_FILE", STRING)
--  , ("OS",STRING)
  , ("UNAME",STRING)
  , ("PROCESSOR",STRING)
  , ("TOPOLOGY",STRING)
  , ("GIT_BRANCH",STRING)
  , ("GIT_HASH",STRING)
  , ("GIT_DEPTH",NUMBER)
  , ("WHO",STRING)
  , ("ETC_ISSUE",STRING)
  , ("LSPCI",STRING)    
  , ("FULL_LOG",STRING)
  -- New fields: [2013.12.01]
  , ("MEDIANTIME_ALLOCRATE", STRING)
  , ("MEDIANTIME_MEMFOOTPRINT", STRING)
  -- New field: [2014.02.19]
  , ("ALLJITTIMES", STRING) -- In order of trials like ALLTIMES.
  ]

-- FIMXE: at least test that resultToTuple returns lits the same
-- length as fusionSchema.
 

benchmarkResultToSchema :: BenchmarkResult -> [(String, CellType)]
benchmarkResultToSchema bm = fusionSchema ++ map custom (_CUSTOM bm) 
  where
    custom (tag, IntResult _) = (tag,NUMBER)
    custom (tag, DoubleResult _) = (tag,NUMBER)
    custom (tag, StringResult _) = (tag, STRING) 


-- | The type of Fusion table plugins.  Currently this is a singleton type; there is
-- really only one fusion plugin.
data FusionPlug = FusionPlug
  deriving (Eq,Show,Ord,Read)

instance Plugin FusionPlug where
  type PlugConf FusionPlug = FusionConfig
  type PlugFlag FusionPlug = FusionCmdLnFlag

  -- | Better be globally unique!  Careful.
  plugName _ = "fusion" 
  --  plugName _ = "Google_FusionTable_Backend"

  plugCmdOpts _ = fusion_cli_options

  plugUploadRow _ cfg row = runReaderT (uploadBenchResult row) cfg

  plugInitialize p gconf = do 
   putStrLn " [fusiontable] Fusion table plugin initializing.. First, find config."
   gc2 <- let fc@FusionConfig{fusionClientID, fusionClientSecret, fusionTableID} =
                  getMyConf p gconf in 
          case (benchsetName gconf, fusionTableID) of
            (Nothing,Nothing) -> error "No way to find which fusion table to use!  No name given and no explicit table ID."
            (_, Just _tid)     -> return gconf
            (Just name,_) -> do
              case (fusionClientID, fusionClientSecret) of
                (Just cid, Just sec ) -> do
                  let auth = OAuth2Client { clientId=cid, clientSecret=sec }
                  (tid,cols) <- runReaderT (getTableId auth name) gconf
                  putStrLn$ " [fusiontable] -> Resolved name "++show name++" to table ID " ++show tid
                  return $! setMyConf p fc{ fusionTableID= Just tid, serverColumns= cols } gconf 
                (_,_) -> error "When --fusion-upload is activated --clientid and --clientsecret are required (or equiv ENV vars)"
   let fc2 = getMyConf p gc2
   let (Just cid, Just sec) = (fusionClientID fc2, fusionClientSecret fc2)
       authclient = OAuth2Client { clientId = cid, clientSecret = sec }
   putStrLn " [fusiontable] Second, lets retrieved cached auth tokens on the file system..."
   _toks <- getCachedTokens authclient

   -- TEMP: This should become another command line flag: --fusion-list to list the tables.
{-
   putStrLn " [fusiontable] Next, to test our connections, attempt to list tables:"
   strs <- fmap (map tab_name) (listTables (B.pack (accessToken toks)))
   putStrLn$" [fusiontable] All of users tables:\n"++ unlines (map ("   "++) strs)
-}
   return gc2

  foldFlags _p flgs cnf0 = 
      foldr ($) cnf0 (map doFlag flgs)
    where      
      -- TODO: Move this one to the global config
      doFlag FusionTest r = r
      doFlag (ClientID cid) r = r { fusionClientID = Just cid } 
      doFlag (ClientSecret s) r = r { fusionClientSecret = Just s } 
      doFlag (FusionTables m) r = 
--         let r2 = r { doFusionUpload = True } in
         case m of 
           Just tid -> r { fusionTableID = Just tid } 
           Nothing  -> r


-- | All the command line options understood by this plugin.
fusion_cli_options :: (String, [OptDescr FusionCmdLnFlag])
fusion_cli_options =
  ("Fusion Table Options:",
      [ Option [] ["fusion-upload"] (OptArg FusionTables "TABLEID")
        "enable fusion table upload.  Optionally set TABLEID; otherwise create/discover it."
      , Option [] ["clientid"]     (ReqArg ClientID "ID") 
        ("Use (and cache auth tokens for) Google client ID\n"++
         "Alternatively set by env var HSBENCHER_GOOGLE_CLIENTID")
      , Option [] ["clientsecret"] (ReqArg ClientSecret "STR")
        ("Use Google client secret\n"++
         "Alternatively set by env var HSBENCHER_GOOGLE_CLIENTSECRET")
      , Option [] ["fusion-test"]  (NoArg FusionTest)   "Test authentication and list tables if possible." 
      ])

-- | Parsed command line options provided by the user initiating benchmarking.
data FusionCmdLnFlag = 
   FusionTables (Maybe TableId)
 | ClientID     String
 | ClientSecret String
 | FusionTest
 deriving (Show,Read,Ord,Eq, Typeable)


