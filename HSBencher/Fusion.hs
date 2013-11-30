{-# LANGUAGE NamedFieldPuns, RecordWildCards, ScopedTypeVariables #-}

-- | Code pertaining to Google Fusion Table upload.
--   Built conditionally based on the -ffusion flag.

module HSBencher.Fusion
       ( FusionConfig(..), stdRetry, getTableId
       , fusionSchema, resultToTuple
       , uploadBenchResult
       )
       where

import Control.Monad.Reader
import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Data.Maybe (isJust, fromJust, catMaybes)
import qualified Data.ByteString.Char8 as B
-- import Network.Google (retryIORequest)
import Network.Google.OAuth2 (getCachedTokens, refreshTokens, OAuth2Client(..), OAuth2Tokens(..))
import Network.Google.FusionTables (createTable, listTables, listColumns, bulkImportRows
                                    TableId, CellType(..), TableMetadata(..))
import Network.HTTP.Conduit (HttpException)
import HSBencher.Types
import HSBencher.Logging (log)
import Prelude hiding (log)

----------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------

-- defaultColumns =
--   ["Program","Args","Threads","Sched","Threads",
--    "MinTime","MedianTime","MaxTime", "MinTime_Prod","MedianTime_Prod","MaxTime_Prod"]


-- | The standard retry behavior when receiving HTTP network errors.
stdRetry :: String -> OAuth2Client -> OAuth2Tokens -> IO a ->
            BenchM a
stdRetry msg client toks action = do
  conf <- ask
  let retryHook exn = runReaderT (do
        log$ " [fusiontable] Retrying during <"++msg++"> due to HTTPException: " ++ show exn
        log$ " [fusiontable] Retrying, but first, attempt token refresh..."
        -- QUESTION: should we retry the refresh itself, it is NOT inside the exception handler.
        -- liftIO$ refreshTokens client toks
        -- liftIO$ retryIORequest (refreshTokens client toks) (\_ -> return ()) [1,1]
        stdRetry "refresh tokens" client toks (refreshTokens client toks)
        return ()
                                 ) conf
  liftIO$ retryIORequest action retryHook [1,2,4,8,16,32,64]


-- | Takes an idempotent IO action that includes a network request.  Catches
-- `HttpException`s and tries a gain a certain number of times.  The second argument
-- is a callback to invoke every time a retry occurs.
-- 
-- Takes a list of *seconds* to wait between retries.  A null list means no retries,
-- an infinite list will retry indefinitely.  The user can choose whatever temporal
-- pattern they desire (e.g. exponential backoff).
--
-- Once the retry list runs out, the last attempt may throw `HttpException`
-- exceptions that escape this function.
retryIORequest :: IO a -> (HttpException -> IO ()) -> [Double] -> IO a
retryIORequest req retryHook times = loop times
  where
    loop [] = req
    loop (delay:tl) = 
      E.catch req $ \ (exn::HttpException) -> do 
        retryHook exn
        threadDelay (round$ delay * 1000 * 1000) -- Microseconds
        loop tl


-- | Get the table ID that has been cached on disk, or find the the table in the users
-- Google Drive, or create a new table if needed.
getTableId :: OAuth2Client -> String -> BenchM TableId
getTableId auth tablename = do
  log$ " [fusiontable] Fetching access tokens, client ID/secret: "++show (clientId auth, clientSecret auth)
  toks      <- liftIO$ getCachedTokens auth
  log$ " [fusiontable] Retrieved: "++show toks
  let atok  = B.pack $ accessToken toks
  allTables <- stdRetry "listTables" auth toks $ listTables atok
  log$ " [fusiontable] Retrieved metadata on "++show (length allTables)++" tables"

  case filter (\ t -> tab_name t == tablename) allTables of
    [] -> do log$ " [fusiontable] No table with name "++show tablename ++" found, creating..."
             TableMetadata{tab_tableId} <- stdRetry "createTable" auth toks $
                                           createTable atok tablename fusionSchema
             log$ " [fusiontable] Table created with ID "++show tab_tableId

             -- TODO: IF it exists but doesn't have all the columns, then add the necessary columns.
             
             return tab_tableId
    [t] -> do log$ " [fusiontable] Found one table with name "++show tablename ++", ID: "++show (tab_tableId t)
              return (tab_tableId t)
    ls  -> error$ " More than one table with the name '"++show tablename++"' !\n "++show ls



uploadBenchResult :: BenchmarkResult -> BenchM ()
uploadBenchResult  br@BenchmarkResult{..} = do
    Config{fusionConfig} <- ask
    let FusionConfig{fusionClientID, fusionClientSecret, fusionTableID} = fusionConfig
    let (Just cid, Just sec) = (fusionClientID, fusionClientSecret)
        authclient = OAuth2Client { clientId = cid, clientSecret = sec }
    -- FIXME: it's EXTREMELY inefficient to authenticate on every tuple upload:
    toks  <- liftIO$ getCachedTokens authclient
    let tuple = resultToTuple br
    let (cols,vals) = unzip tuple
    log$ " [fusiontable] Uploading row with "++show (length cols)++
         " columns containing "++show (sum$ map length vals)++" characters of data"

    -- It's easy to blow the URL size; we need the bulk import version.
    --    stdRetry "insertRows" authclient toks $ insertRows
    stdRetry "bulkImportRows" authclient toks $ bulkImportRows
       (B.pack$ accessToken toks) (fromJust fusionTableID) cols [vals]
    log$ " [fusiontable] Done uploading, run ID "++ (fromJust$ lookup "RUNID" tuple)
         ++ " date "++ (fromJust$ lookup "DATETIME" tuple)
--       [[testRoot, unwords args, show numthreads, t1,t2,t3, p1,p2,p3]]
    return ()           


-- | A representaton used for creating tables.  Must be isomorphic to
-- `BenchmarkResult`.  This could perhaps be generated automatically.
fusionSchema :: [(String, CellType)]
fusionSchema =
  [ ("PROGNAME",STRING)
  , ("VARIANT",STRING)
  , ("ARGS",STRING)    
  , ("HOSTNAME",STRING)
  -- The run is identified by hostname_secondsSinceEpoch:
  , ("RUNID",STRING)
  , ("CI_BUILD_ID",STRING)  
  , ("THREADS",NUMBER)
  , ("DATETIME",DATETIME)    
  , ("MINTIME", NUMBER)
  , ("MEDIANTIME", NUMBER)
  , ("MAXTIME", NUMBER)
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
  , ("MINTIME_PRODUCTIVITY",    show$ _MINTIME_PRODUCTIVITY r)
  , ("MEDIANTIME_PRODUCTIVITY", show$ _MEDIANTIME_PRODUCTIVITY r)
  , ("MAXTIME_PRODUCTIVITY",    show$ _MAXTIME_PRODUCTIVITY r)
  , ("ALLTIMES",       _ALLTIMES r)
  , ("TRIALS",   show$ _TRIALS r)
  , ("COMPILER",       _COMPILER r)
  , ("COMPILE_FLAGS",  _COMPILE_FLAGS r)
  , ("RUNTIME_FLAGS",  _RUNTIME_FLAGS r)
  , ("ENV_VARS",       _ENV_VARS r)
  , ("BENCH_VERSION",  _BENCH_VERSION r)
  , ("BENCH_FILE",     _BENCH_FILE r)
  , ("UNAME",          take 20 (_UNAME r)) -- TEMP, FIXME
  , ("PROCESSOR",      _PROCESSOR r)
  , ("TOPOLOGY",       _TOPOLOGY r)
  , ("GIT_BRANCH",     _GIT_BRANCH r)
  , ("GIT_HASH",       _GIT_HASH r)
  , ("GIT_DEPTH", show$ _GIT_DEPTH r)
  , ("WHO",            take 20 (_WHO r))   -- TEMP, FIXME
  , ("ETC_ISSUE", _ETC_ISSUE r)
  , ("LSPCI", _LSPCI r)    
  , ("FULL_LOG", _FULL_LOG r)
  ]
  
