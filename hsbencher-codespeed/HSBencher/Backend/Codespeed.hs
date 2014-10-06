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
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format ()


import Network.HTTP.Conduit (HttpException, Request(..), http, httpLbs, parseUrl, newManager, conduitManagerSettings)
import Network.HTTP.Conduit (Response(..), RequestBody(..))
-- import Network.HTTP.Conduit (Manager, Request(..), RequestBody(..), Response(..), HttpException, 
--                              closeManager, def, httpLbs, newManager, responseBody)
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

-- TODO: may need to grab stdRetry / retryIORequest from the Fusion plugin..

-- | Configuration options for Codespeed uploading.
data CodespeedConfig = 
  CodespeedConfig { codespeedURL :: URL }
  deriving (Show,Read,Ord,Eq, Typeable)

-- | Parsed command line options provided by the user that initiaties benchmarking.
data CodespeedCmdLnFlag = CodespeedURL URL
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
  conf <- ask
  -- Look up our configuration dynamically based the plugin type:
  let codespeedConfig = getMyConf CodespeedPlug conf
  let CodespeedConfig {codespeedURL} = codespeedConfig
  lift $ runResourceT $ do 
    req0    <- liftIO$ parseUrl (codespeedURL ++ "/result/add")
    let bod = renderJSONResult br
        req = req0 { method = "POST"
                   , secure = False
                   , requestBody = RequestBodyLBS bod
                   }
    manager <- liftIO$ newManager conduitManagerSettings
    res     <- httpLbs req manager
    -- We could do something with the result (ByteString), but as long
    -- as its not error, we don't care:
    let _bstr = responseBody res
    return ()
  return ()

renderJSONResult :: BenchmarkResult -> B.ByteString
renderJSONResult _ = -- BenchmarkResult{} = 
  B.pack $ encodeStrict $ JSObject $ toJSObject $ 
   [ ("commitid", showJSON (14::Int))
   , ("branch",      s "blah")
   , ("project",     s "Accelerate")
   , ("executable",  s "myexe 04 32bits")
   , ("benchmark",   s "float")
   , ("environment", s "cutter")
   , ("result_value", d 3.8)
   ] ++ 
   -- Plus add optional fields:
   [
--   , ("revision_date", s "")  -- Optional. Default is taken either
--                            -- from VCS integration or from current date
--   , ("result_date", s "")    -- Optional, default is current date
--   , ("std_dev", showJSON (1.11111 :: Double))  -- Optional. Default is blank
--   , ("max", d 4001.6)  -- Optional. Default is blank
--   , ("min", d 3995.1)  -- Optional. Default is blank    
   -- RRN: Question: are max and min the observed max and min presumably?
   ]
 where 
  d :: Double -> JSValue
  d = showJSON
  s = JSString . toJSString

-- # Optional fields
-- data.update({
-- })



   -- req = appendHeaders [("Content-Type", "application/json")] $
   --        appendBody (BL.pack json)
   --        (makeRequest tok fusiontableApi "POST"
   --          (fusiontableHost, "fusiontables/v1/tables" ))
{-
makeRequest ::
     AccessToken       -- ^ The OAuth 2.0 access token.
  -> (String, String)  -- ^ The Google API name and version.
  -> String            -- ^ The HTTP method.
  -> (String, String)  -- ^ The host and path for the request.
  -> Request m         -- ^ The HTTP request.
makeRequest accessToken (apiName, apiVersion) method (host, path) =
  -- TODO: In principle, we should UTF-8 encode the bytestrings packed below.
  def {
    method = BS8.pack method
  , secure = True
  , host = BS8.pack host
  , port = 443
  , path = BS8.pack path
  , requestHeaders = [
      (makeHeaderName apiName, BS8.pack apiVersion)
    , (makeHeaderName "Authorization",  BS8.append (BS8.pack "OAuth ") accessToken)
    ]
  }
-}



-- | The type of Codespeed table plugins.  Currently this is a singleton type; there is
-- really only one Codespeed plugin.
data CodespeedPlug = CodespeedPlug
  deriving (Eq,Show,Ord,Read)

instance Plugin CodespeedPlug where
  -- These configs are stored in a dynamically typed list within the global BenchM config:
  type PlugConf CodespeedPlug = CodespeedConfig
  type PlugFlag CodespeedPlug = CodespeedCmdLnFlag

  defaultPlugConf _ = CodespeedConfig 
    { codespeedURL  = error "Must set Codespeed URL to use this plugin!"
    }

  -- | Better be globally unique!  Careful.
  plugName _ = "codespeed" 
  plugCmdOpts _ = codespeed_cli_options
  plugUploadRow p cfg row = runReaderT (uploadBenchResult row) cfg
  plugInitialize p gconf = do
   putStrLn " [codespeed] Codespeed table plugin initializing.. (which is a NOOP)"
   return gconf

  foldFlags p flgs cnf0 = 
      foldr ($) cnf0 (map doFlag flgs)
    where      
      doFlag (CodespeedURL url) r = r { codespeedURL = url} 

theEnv :: [(String,String)] 
theEnv = unsafePerformIO getEnvironment

-- | All the command line options understood by this plugin.
codespeed_cli_options :: (String, [OptDescr CodespeedCmdLnFlag])
codespeed_cli_options =
  ("Codespeed Table Options:",
      [ Option [] ["codespeed"] (ReqArg CodespeedURL "URL")
        "specify the root URL of the Codespeed installation"
      ])

