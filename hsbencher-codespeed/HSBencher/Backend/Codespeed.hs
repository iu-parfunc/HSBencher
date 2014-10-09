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
  let json = renderJSONResult br
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


renderJSONResult :: BenchmarkResult -> String
renderJSONResult _br = 
  simpleFormat
  [ ("project",     S "MyProject")
   , ("executable",  S "myexe 04 32bits")
   , ("benchmark",   S "float")
   , ("commitid",    S "8")
   , ("environment", S "cutter")
   , ("result_value", D 2500.1)
   , ("branch",      S "default")
   -- Plus add optional fields:
--   , ("revision_date", s "")  -- Optional. Default is taken either
--                            -- from VCS integration or from current date
--   , ("result_date", s "")    -- Optional, default is current date
--   , ("std_dev", showJSON (1.11111 :: Double))  -- Optional. Default is blank
--   , ("max", d 4001.6)  -- Optional. Default is blank
--   , ("min", d 3995.1)  -- Optional. Default is blank    
   -- RRN: Question: are max and min the observed max and min presumably?
   ]

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

  defaultPlugConf _ = CodespeedConfig 
--    { codespeedURL  = error "Must set Codespeed URL to use this plugin!"
    { codespeedURL  = "http://unknown address of codespeed server -- please set it to use this plugin"
    }

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

