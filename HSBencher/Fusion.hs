{-# LANGUAGE NamedFieldPuns, RecordWildCards, ScopedTypeVariables, CPP, BangPatterns #-}
{-# LANGUAGE TupleSections, DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

-- | Google Fusion Table upload of benchmark data.
--   Built conditionally based on the -ffusion flag.
--
--   Be careful!  This module will appear empty if hsbencher is not built with that
--   flag.  In the future we will probably switch to a new architecture that allows
--   us to factor this out as its own package.

module HSBencher.Fusion
#ifndef FUSION_TABLES
       () where
#else
       ( FusionConfig(..), stdRetry, getTableId
       , fusionSchema, resultToTuple
       , uploadBenchResult
       , FusionPlug(..), FusionCmdLnFlag(..),
--       , fusionPlugin, fusionUploader
       )
       where

import Control.Monad.Reader
import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Data.Maybe (isJust, fromJust, catMaybes, fromMaybe)
import Data.Dynamic
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format ()
-- import Network.Google (retryIORequest)
import Network.Google.OAuth2 (getCachedTokens, refreshTokens, OAuth2Client(..), OAuth2Tokens(..))
import Network.Google.FusionTables (createTable, createColumn, listTables, listColumns,
                                    bulkImportRows, insertRows,
                                    TableId, CellType(..), TableMetadata(..), ColumnMetadata(..))
import Network.HTTP.Conduit (HttpException)
import HSBencher.Types
import HSBencher.Logging (log)
import Prelude hiding (log)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Console.GetOpt (getOpt, ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.Directory (doesFileExist, doesDirectoryExist, getAppUserDataDirectory,
                         createDirectory, renameFile, removeFile)
import System.FilePath ((</>),(<.>), splitExtension)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getEnvironment)
import Control.Concurrent.MVar

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


-- | Get the table ID that has been cached on disk, or find the the table in the users
-- Google Drive, or create a new table if needed.
--
-- In the case of a preexisting table, this function also performs sanity checking
-- comparing the expected schema (including column ordering) to the sserver side one.
-- It returns the permutation of columns found server side.
getTableId :: OAuth2Client -> String -> BenchM (TableId, [String])
getTableId auth tablename = do
  log$ " [fusiontable] Fetching access tokens, client ID/secret: "++show (clientId auth, clientSecret auth)
  toks      <- liftIO$ getCachedTokens auth
  log$ " [fusiontable] Retrieved: "++show toks
  let atok  = B.pack $ accessToken toks
  Just allTables <- stdRetry "listTables" auth toks $ listTables atok
  log$ " [fusiontable] Retrieved metadata on "++show (length allTables)++" tables"

  let ourSchema = map fst fusionSchema
      ourSet    = S.fromList ourSchema
  case filter (\ t -> tab_name t == tablename) allTables of
    [] -> do log$ " [fusiontable] No table with name "++show tablename ++" found, creating..."
             Just TableMetadata{tab_tableId} <- stdRetry "createTable" auth toks $
                                                createTable atok tablename fusionSchema
             log$ " [fusiontable] Table created with ID "++show tab_tableId
             
             -- TODO: IF it exists but doesn't have all the columns, then add the necessary columns.
             return (tab_tableId, ourSchema)
    [t] -> do let tid = (tab_tableId t)
              log$ " [fusiontable] Found one table with name "++show tablename ++", ID: "++show tid
              log$ " [fusiontable] Checking columns... "              
              targetSchema <- fmap (map col_name) $ liftIO$ listColumns atok tid
              let targetSet = S.fromList targetSchema
                  missing   = S.difference ourSet targetSet
                  misslist  = L.filter (`S.member` missing) ourSchema -- Keep the order.
                  extra     = S.difference targetSet ourSet
              unless (targetSchema == ourSchema) $ 
                log$ "WARNING: HSBencher upload schema (1) did not match server side schema (2):\n (1) "++
                     show ourSchema ++"\n (2) " ++ show targetSchema
                     ++ "\n HSBencher will try to make do..."
              unless (S.null missing) $ do                
                log$ "WARNING: These fields are missing server-side, creating them: "++show misslist
                forM_ misslist $ \ colname -> do
                  ColumnMetadata{col_name, col_columnId} <- liftIO$ createColumn atok tid (colname, STRING)
                  log$ "   -> Created column with name,id: "++show (col_name, col_columnId)
              unless (S.null extra) $ do
                log$ "WARNING: The fusion table has extra fields that HSBencher does not know about: "++
                     show (S.toList extra)
                log$ "         Expect null-string entries in these fields!  "
              -- For now we ASSUME that new columns are added to the end:
              -- TODO: We could do another read from the list of columns to confirm.
              return (tid, targetSchema ++ misslist)
    ls  -> error$ " More than one table with the name '"++show tablename++"' !\n "++show ls


-- TEMP: Hack
fileLock :: MVar ()
fileLock = unsafePerformIO (newMVar ())
-- TODO/FIXME: Make this configurable.

-- | Push the results from a single benchmark to the server.
uploadBenchResult :: BenchmarkResult -> BenchM ()
uploadBenchResult  br@BenchmarkResult{..} = do
--    Config{fusionConfig} <- ask
    fusionConfig <- error "FINISHME - acquire config dynamically"
    let FusionConfig{fusionClientID, fusionClientSecret, fusionTableID, serverColumns} = fusionConfig
    let (Just cid, Just sec) = (fusionClientID, fusionClientSecret)
        authclient = OAuth2Client { clientId = cid, clientSecret = sec }
    -- FIXME: it's EXTREMELY inefficient to authenticate on every tuple upload:
    toks  <- liftIO$ getCachedTokens authclient
    let ourData = M.fromList $ resultToTuple br
        -- Any field HSBencher doesn't know about just gets an empty string:
        tuple   = [ (key, fromMaybe "" (M.lookup key ourData))
                  | key <- serverColumns ]
        (cols,vals) = unzip tuple
    log$ " [fusiontable] Uploading row with "++show (length cols)++
         " columns containing "++show (sum$ map length vals)++" characters of data"

    cabalD <- lift $ getAppUserDataDirectory "cabal"
    let csvfile = cabalD </> "hsbencherDribble.csv"
    log$ " [fusiontable] TEMP: Also dumping data to: "++csvfile
    lift $ withMVar fileLock $ \ () -> do 
       b <- doesFileExist csvfile
       unless b$ writeFile csvfile (concat (L.intersperse "," cols)++"\n")
       appendFile csvfile (concat (L.intersperse "," (map show vals))++"\n")

    -- It's easy to blow the URL size; we need the bulk import version.
    -- stdRetry "insertRows" authclient toks $ insertRows
    res <- stdRetry "bulkImportRows" authclient toks $ bulkImportRows
            (B.pack$ accessToken toks) (fromJust fusionTableID) cols [vals]
    case res of 
      Just _  -> log$ " [fusiontable] Done uploading, run ID "++ (fromJust$ lookup "RUNID" tuple)
                      ++ " date "++ (fromJust$ lookup "DATETIME" tuple)
      Nothing -> log$ " [fusiontable] WARNING: Upload failed the maximum number of times.  Continuing with benchmarks anyway"
-- TODO/FIXME: Make this configurable.
    return ()           


-- | A representaton used for creating tables.  Must be isomorphic to
-- `BenchmarkResult`.  This could perhaps be generated automatically.
-- 
-- Note, order is important here, because this is the preferred order we'd like to
-- have it in the Fusion table.
fusionSchema :: [(String, CellType)]
fusionSchema =
  [ ("PROGNAME",STRING)
  , ("VARIANT",STRING)
  , ("ARGS",STRING)    
  , ("HOSTNAME",STRING)
  , ("MINTIME", NUMBER)
  , ("MEDIANTIME", NUMBER)
  , ("MAXTIME", NUMBER)
  -- The run is identified by hostname_secondsSinceEpoch:
  , ("RUNID",STRING)
  , ("CI_BUILD_ID",STRING)  
  , ("THREADS",NUMBER)
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

-- | Convert the Haskell representation of a benchmark result into a tuple for Fusion
-- table upload.
resultToTuple :: BenchmarkResult -> [(String,String)]
resultToTuple r =
  [ ("PROGNAME", _PROGNAME r)
  , ("VARIANT",  _VARIANT r)
  , ("ARGS",     unwords$ _ARGS r)    
  , ("HOSTNAME", _HOSTNAME r)
  , ("RUNID",    _RUNID r)
  , ("CI_BUILD_ID", _CI_BUILD_ID r)    
  , ("THREADS",  show$ _THREADS r)
  , ("DATETIME", _DATETIME r)
  , ("MINTIME",     show$ _MINTIME r)
  , ("MEDIANTIME",  show$ _MEDIANTIME r)
  , ("MAXTIME",     show$ _MAXTIME r)
  , ("MINTIME_PRODUCTIVITY",    fromMaybe "" $ fmap show $ _MINTIME_PRODUCTIVITY r)
  , ("MEDIANTIME_PRODUCTIVITY", fromMaybe "" $ fmap show $ _MEDIANTIME_PRODUCTIVITY r)
  , ("MAXTIME_PRODUCTIVITY",    fromMaybe "" $ fmap show $ _MAXTIME_PRODUCTIVITY r)
  , ("ALLTIMES",       _ALLTIMES r)
  , ("TRIALS",   show$ _TRIALS r)
  , ("COMPILER",       _COMPILER r)
  , ("COMPILE_FLAGS",  _COMPILE_FLAGS r)
  , ("RUNTIME_FLAGS",  _RUNTIME_FLAGS r)
  , ("ENV_VARS",       _ENV_VARS r)
  , ("BENCH_VERSION",  _BENCH_VERSION r)
  , ("BENCH_FILE",     _BENCH_FILE r)
  , ("UNAME",          _UNAME r)
  , ("PROCESSOR",      _PROCESSOR r)
  , ("TOPOLOGY",       _TOPOLOGY r)
  , ("GIT_BRANCH",     _GIT_BRANCH r)
  , ("GIT_HASH",       _GIT_HASH r)
  , ("GIT_DEPTH", show$ _GIT_DEPTH r)
  , ("WHO",            _WHO r)
  , ("ETC_ISSUE", _ETC_ISSUE r)
  , ("LSPCI", _LSPCI r)    
  , ("FULL_LOG", _FULL_LOG r)
  , ("MEDIANTIME_ALLOCRATE",    fromMaybe "" $ fmap show $ _MEDIANTIME_ALLOCRATE r)
  , ("MEDIANTIME_MEMFOOTPRINT", fromMaybe "" $ fmap show $ _MEDIANTIME_MEMFOOTPRINT r)    
  , ("ALLJITTIMES", _ALLJITTIMES r)
  ]

#if 0 

fusionPlugin :: Plugin
fusionPlugin = Plugin
 { plugName = "fusionPlugin"
 , plugUsageInfo = unlines 
   [ "     HSBENCHER_GOOGLE_CLIENTID, HSBENCHER_GOOGLE_CLIENTSECRET: if FusionTable upload is enabled, the",
     "               client ID and secret can be provided by env vars OR command line options. " ]
 , plugInit = \ opts -> return ()
 , plugCmdOptions = dynOptDescrs
 , plugUploader = fusionUploader
 }
 where
  dynOptDescrs :: [OptDescr Dynamic]
  dynOptDescrs = map (fmap toDyn) $ snd fusion_cli_options

fusionUploader :: Uploader
fusionUploader = Uploader 
 { upname = "fusionTableUploader"
 , upload = uploadBenchResult 
 }

#endif

data FusionPlug = FusionPlug
  deriving (Eq,Show,Ord,Read)

instance PlugIn FusionPlug where
  type PlugConf FusionPlug = FusionConfig
  type PlugFlag FusionPlug = FusionCmdLnFlag

  defaultPlugConf _ = FusionConfig 
    { fusionTableID  = Nothing 
    , fusionClientID     = lookup "HSBENCHER_GOOGLE_CLIENTID" theEnv
    , fusionClientSecret = lookup "HSBENCHER_GOOGLE_CLIENTSECRET" theEnv
    , serverColumns      = []
    }

  plugInitialize FusionPlug Config{} = do 
   putStrLn " [hsbencher] Fusion table plugin initializing..."
#if 0
   let FusionConfig{fusionClientID, fusionClientSecret, fusionTableID} = fusionConfig conf1
   let (Just cid, Just sec) = (fusionClientID, fusionClientSecret)
       authclient = OAuth2Client { clientId = cid, clientSecret = sec }
   putStrLn "[hsbencher] Fusion table test mode.  Getting tokens:"
   toks  <- getCachedTokens authclient
   putStrLn$ "[hsbencher] Successfully got tokens: "++show toks
   putStrLn "[hsbencher] Next, attempt to list tables:"
   strs <- fmap (map tab_name) (listTables (B.pack (accessToken toks)))
   putStrLn$"[hsbencher] All of users tables:\n"++ unlines (map ("   "++) strs)
   exitSuccess
#endif
   return ()

theEnv :: [(String,String)] 
theEnv = unsafePerformIO getEnvironment


fusion_cli_options :: (String, [OptDescr FusionCmdLnFlag])
fusion_cli_options =
  ("\n Fusion Table Options:",
      [ Option [] ["fusion-upload"] (OptArg FusionTables "TABLEID")
        "enable fusion table upload.  Optionally set TABLEID; otherwise create/discover it."

      , Option [] ["name"]         (ReqArg BenchsetName "NAME") "Name for created/discovered fusion table."
      , Option [] ["clientid"]     (ReqArg ClientID "ID")     "Use (and cache) Google client ID"
      , Option [] ["clientsecret"] (ReqArg ClientSecret "STR") "Use (and cache) Google client secret"
      , Option [] ["fusion-test"]  (NoArg FusionTest)   "Test authentication and list tables if possible." 
      ])

data FusionCmdLnFlag = 
   FusionTables (Maybe TableId)
 | BenchsetName (String)
 | ClientID     String
 | ClientSecret String
 | FusionTest
 deriving (Show,Read,Ord,Eq, Typeable)

data FusionConfig = 
  FusionConfig
  { fusionTableID  :: Maybe TableId -- ^ This must be Just whenever doFusionUpload is true.
  , fusionClientID :: Maybe String
  , fusionClientSecret :: Maybe String
  , serverColumns  :: [String] -- ^ Record the ordering of columns server side.
  }
  deriving (Show,Read,Ord,Eq, Typeable)

#if 0 
      doFlag (BenchsetName name) r     = r { benchsetName= Just name }
      doFlag (ClientID cid)   r = let r2 = fusionConfig r in
                                  r { fusionConfig= r2 { fusionClientID = Just cid } }
      doFlag (ClientSecret s) r = let r2 = fusionConfig r in
                                  r { fusionConfig= r2 { fusionClientSecret = Just s } }
      doFlag (FusionTables m) r = 
         let r2 = r { doFusionUpload = True } in
         case m of 
           Just tid -> let r3 = fusionConfig r in
                       r2 { fusionConfig= r3 { fusionTableID = Just tid } }
           Nothing -> r2
      doFlag FusionTest r = r
#endif

#if 0
  finalconf <- if not (doFusionUpload conf) then return conf else
               let fconf = fusionConfig conf in
               case (benchsetName conf, fusionTableID fconf) of
                (Nothing,Nothing) -> error "No way to find which fusion table to use!  No name given and no explicit table ID."
                (_, Just tid) -> return conf
                (Just name,_) -> do
                  case (fusionClientID fconf, fusionClientSecret fconf) of
                    (Just cid, Just sec ) -> do
                      let auth = OAuth2Client { clientId=cid, clientSecret=sec }
                      (tid,cols) <- runReaderT (getTableId auth name) conf
                      return conf{ fusionConfig= fconf { fusionTableID= Just tid
                                                       , serverColumns= cols }}
                    (_,_) -> error "When --fusion-upload is activated --clientid and --clientsecret are required (or equiv ENV vars)"
#endif


#if 0
 , fusionConfig   :: FusionConfig
#endif



--  , doFusionUpload  :: Bool
-- -- , uploaders       :: [Uploader]

--            , doFusionUpload = False




#endif
-- End ifndef FUSION_TABLES

