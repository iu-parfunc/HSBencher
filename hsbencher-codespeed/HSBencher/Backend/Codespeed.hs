{-# LANGUAGE NamedFieldPuns, RecordWildCards, ScopedTypeVariables, CPP, BangPatterns #-}
{-# LANGUAGE TupleSections, DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | CodeSpeed website upload of benchmark data.
-- 
--   This module must be used in conjunction with the main "hsbencher" package,
--   e.g. "import HSBencher", and then "import HSBencher.Backend.CodeSpeed" and 
--   add the plugin 

module HSBencher.Backend.Codespeed
       ( -- * The plugin itself, what you probably want 
         defaultCodespeedPlugin

         -- * Details and configuration options.
       , CodespeedConfig(..)
       -- , stdRetry, getTableId
       -- , fusionSchema, resultToTuple
       -- , uploadBenchResult
       , CodespeedPlug(), CodespeedCmdLnFlag(..),
       )
       where

import Control.Monad.Reader
import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Data.Maybe (isJust, fromJust, catMaybes, fromMaybe)
import Data.Dynamic
import Data.Default (Default(..))
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format ()

import Network.HTTP.Types (renderQuery, urlEncode, urlDecode)
import Network.HTTP (simpleHTTP, postRequestWithBody)

import Control.Monad.Trans.Resource (runResourceT)
import Text.JSON -- (encodeStrict, toJSObject)

import HSBencher.Types
import HSBencher.Internal.Logging (log)
import Prelude hiding (log)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Console.GetOpt (getOpt, ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.Directory (doesFileExist, doesDirectoryExist, getAppUserDataDirectory,
                         createDirectory, renameFile, removeFile)
import System.FilePath ((</>),(<.>), splitExtension)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getEnvironment)
import System.Exit
import Control.Concurrent.MVar
--------------------------------------------------------------------------------


-- | A default plugin.  This binding provides future-proof way to get
--   a default instance of the plugin, in the eventuality that more
--   configuration options are added in the future.
defaultCodespeedPlugin :: CodespeedPlug
defaultCodespeedPlugin = CodespeedPlug

-- | This is the same as defaultCodespeedPlugin
instance Default CodespeedPlug where 
  def = defaultCodespeedPlugin

-- TODO: may need to grab stdRetry / retryIORequest from the Fusion plugin..

-- | Configuration options for Codespeed uploading.
data CodespeedConfig = 
  CodespeedConfig { codespeedURL :: URL
                  , projName     :: String }
  deriving (Show,Read,Ord,Eq, Typeable)

-- | Note, the default config may not be complete and thus may have
--   some required fields to fill in, or errors will ensue.
instance Default CodespeedConfig where 
  def = CodespeedConfig 
    { codespeedURL  = error "incomplete CodespeedConfig: Must set Codespeed URL (--codespeed) to use this plugin!"
    , projName      = error "incomplete CodespeedConfig: Must set Codespeed --projname to use this plugin!"
    }

-- | Parsed command line options provided by the user that initiaties benchmarking.
data CodespeedCmdLnFlag = CodespeedURL URL
                        | CodespeedProjName String
 -- TODO: Authentication!
 deriving (Show,Read,Ord,Eq, Typeable)

type URL = String


getDateTime :: IO String
getDateTime = do 
  utc <- getCurrentTime
  return $ show utc


-- | Push the results from a single benchmark to the server.
uploadBenchResult :: BenchmarkResult -> BenchM ()
uploadBenchResult br = do 
  lift$ putStrLn " [codespeed] Begin upload of one benchmark result."
  conf <- ask
  -- Look up our configuration dynamically based the plugin type:
  let codespeedConfig = getMyConf CodespeedPlug conf

  -- lift$ putStrLn$ " [codespeed] Running with config: \n"++show conf
  lift$ putStrLn$ " [codespeed] Running with plugin config: \n"++show codespeedConfig

  let CodespeedConfig {codespeedURL} = codespeedConfig
      contentType = "application/x-www-form-urlencoded"
--      contentType = "application/json"
      addURL = (codespeedURL ++ "/result/add/json/")

-- Version that uses HTTP pkg:
  let json = renderJSONResult codespeedConfig br
      bod = urlEncode False $ BS.pack json
  let req = postRequestWithBody addURL contentType $ BS.unpack bod
  lift$ putStrLn$ " [codespeed] Uploading json: "++ json
  lift$ putStrLn$ " [codespeed] URl-encoded json POST body: "++ BS.unpack bod
  lift$ putStrLn$ " [codespeed] Submitting HTTP Post request: \n"++show req
  resp <- lift$ simpleHTTP req
  case resp of 
    Left err -> lift$ putStrLn$ " [codespeed] ERROR uploading: \n"++show err
    Right x  -> lift$ putStrLn$ " [codespeed] Got response from server:\n"++show x
  return ()


renderJSONResult :: CodespeedConfig -> BenchmarkResult -> String
renderJSONResult CodespeedConfig{projName} benchRes = 
   -- _PROGNAME _VARIANT _ARGS _HOSTNAME _RUNID _CI_BUILD_ID _THREADS
   -- _DATETIME _MINTIME _MEDIANTIME _MAXTIME _MINTIME_PRODUCTIVITY
   -- _MEDIANTIME_PRODUCTIVITY _MAXTIME_PRODUCTIVITY _ALLTIMES _TRIALS
   -- _COMPILER _COMPILE_FLAGS _RUNTIME_FLAGS _ENV_VARS _BENCH_VERSION
   -- _BENCH_FILE _UNAME _PROCESSOR _TOPOLOGY _GIT_BRANCH _GIT_HASH
   -- _GIT_DEPTH _WHO _ETC_ISSUE _LSPCI _FULL_LOG _MEDIANTIME_ALLOCRATE
   -- _MEDIANTIME_MEMFOOTPRINT _ALLJITTIMES _CUSTOM
  simpleFormat
  [ 
    -- A working example:
       --     ("project",     S "MyProject2")
       --   , ("executable",  S "myexe 04 32bits")
       --   , ("benchmark",   S "float")
       --   , ("commitid",    S "8")
       --   , ("environment", S "cutter")
       --   , ("result_value", D 2500.1)
       --   , ("branch",      S "default")

     ("project",     S projName)
   , ("executable",  S exec)
   , ("benchmark",   S bench)
   , ("commitid",    S _GIT_HASH)
   -- , ("environment", S "129-79-241-98") -- Results in 400 / BAD REQUEST
   -- , ("environment", S "1297924198") -- Results in 400 / BAD REQUEST
   -- Apparently this is the error on the server:
     -- Exception Value:	
     -- Expecting ',' delimiter: line 1 column 235 (char 234)
     -- Exception Location: /opt/python/2.7.8/lib/python2.7/json/decoder.py in raw_decode, line 382
   -- , ("environment", S "hello1297924198") -- Results in 400 / BAD REQUEST
   -- , ("environment", S "hello") -- Also 400 / BAD REQUEST 
         -- Seems to fail if the environment is not REGISTERED already
         -- on the website.  Does not create on demand?
   , ("environment",  S _HOSTNAME) 
   , ("result_value", D _MEDIANTIME)
   , ("branch",       S _GIT_BRANCH)
   -- Plus add optional fields:
--   , ("revision_date", s "")  -- Optional. Default is taken either
--                            -- from VCS integration or from current date
--   , ("result_date", s "")    -- Optional, default is current date
--   , ("std_dev", showJSON (1.11111 :: Double))  -- Optional. Default is blank
   , ("max", D _MAXTIME)  -- Optional. Default is blank
   , ("min", D _MINTIME)  -- Optional. Default is blank    
   -- RRN: Question: are max and min the *observed* max and min presumably?
   ]
 where 
  -- Populate the CodeSpeed fields using the HSBencher fields:
  BenchmarkResult{..} = benchRes
  exec  = combine $ [_VARIANT] ++ if _THREADS==0 then [] 
                                  else [show _THREADS ++ "T"]
  bench = combine [_PROGNAME, unwords _ARGS]

-- | This is a hacky way to pack multiple fields into one field of the
-- destination schema.  There is a tradeoff here between readability
-- and ease of dissection.
combine :: [String] -> String
combine fields = 
  let fields' = filter (not . null) fields in
  L.concat (L.intersperse "|" fields')

data RHS = S String | D Double

-- | The Django-based codespeed server is a bit finicky in exactly
-- what JSON formattincg and URL encodings it accepts.  Thus, rather
-- than using any of the existing frameworks, we just use a particular
-- format we know works.
simpleFormat :: [(String,RHS)] -> String
simpleFormat prs = "json=[{" ++ bod ++"}]"
 where
  bod = L.concat $ L.intersperse ", " $ L.map fn prs
  fn (l,r) = show l ++ ": " ++ rhs r
  rhs (S s) = show s
  rhs (D d) = show d



-- | The type of Codespeed table plugins.  Currently this is a singleton type; there is
-- really only one Codespeed plugin.
data CodespeedPlug = CodespeedPlug
  deriving (Eq,Show,Ord,Read)

instance Plugin CodespeedPlug where
  -- These configs are stored in a dynamically typed list within the global BenchM config:
  type PlugConf CodespeedPlug = CodespeedConfig
  type PlugFlag CodespeedPlug = CodespeedCmdLnFlag

  defaultPlugConf _ = def 

  -- | Better be globally unique!  Careful.
  plugName _    = "codespeed"
  plugCmdOpts _ = codespeed_cli_options
  plugUploadRow p cfg row = runReaderT (uploadBenchResult row) cfg
  plugInitialize p gconf = do
   putStrLn " [codespeed] Codespeed table plugin initializing.. (which is a NOOP)"
   return gconf

  foldFlags p flgs cnf0 = 
      foldr ($) cnf0 (map doFlag flgs)
    where      
      doFlag (CodespeedURL url)     r = r { codespeedURL = url} 
      doFlag (CodespeedProjName nm) r = r { projName = nm }

theEnv :: [(String,String)] 
theEnv = unsafePerformIO getEnvironment

-- | All the command line options understood by this plugin.
codespeed_cli_options :: (String, [OptDescr CodespeedCmdLnFlag])
codespeed_cli_options =
  ("Codespeed Table Options:",
      [ Option [] ["codespeed"] (ReqArg CodespeedURL "URL")
        "specify the root URL of the Codespeed installation"
      , Option [] ["projname"] (ReqArg CodespeedProjName "NAME")
        "specify which Codespeed Project receives the uploaded results"
      ])

