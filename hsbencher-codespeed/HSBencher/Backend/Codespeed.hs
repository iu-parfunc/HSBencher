{-# LANGUAGE NamedFieldPuns, RecordWildCards, ScopedTypeVariables, CPP, BangPatterns #-}
{-# LANGUAGE TupleSections, DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

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
import qualified Data.ByteString.Char8 as B
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format ()
import Network.HTTP.Conduit (HttpException)
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
uploadBenchResult =
  error "FINISHME - codespeed"

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

  plugUploadRow p cfg row = 
    error "FINISHME - codespeed"
    -- runReaderT (uploadBenchResult row) cfg

  plugInitialize p gconf = do 
   putStrLn " [codespeed] Codespeed table plugin initializing.. First, find config."
   error "FINISHME - codespeed"

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

