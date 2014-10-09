{-# LANGUAGE NamedFieldPuns, RecordWildCards, ScopedTypeVariables, CPP, BangPatterns #-}
{-# LANGUAGE TupleSections, DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

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

import Network.HTTP.Conduit (HttpException, Request(..), http, httpLbs, parseUrl, newManager)
import Network.HTTP.Conduit (Response(..), RequestBody(..), def, queryString, urlEncodedBody)
-- import Network.HTTP.Conduit (setQueryString)
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

#if 1
-- Version that uses HTTP pkg:
  let bod = BS.append "json=" $ urlEncode False $ B.toStrict $ renderJSONResult br
      working2 = urlEncode False $ 
                 "json=[{\"project\": \"MyProject\", \"executable\": \"myexe O3 48bits\", \"environment\": \"cutter\", \"benchmark\": \"float\", \"commitid\": \"8\", \"result_value\": 2500.0, \"branch\": \"default\"}]"
      failed3 = urlEncode False $ 
                 "json=[{'project': 'MyProject', 'executable': 'myexe O3 48bits', 'environment': 'cutter', 'benchmark': 'float', 'commitid': '8', 'result_value': 2500.0, 'branch': 'default'}]"

  let req = postRequestWithBody addURL contentType $ BS.unpack $ 
          -- working1 
          working2 
          -- failed3
          -- bod

  lift$ putStrLn$ " [codespeed] working1 == its reincode\n"++ show (working1 == urlEncode False (urlDecode False working1))
  lift$ putStrLn$ " [codespeed] working1:\n"++          show (working1)
  lift$ putStrLn$ " [codespeed] working1 reencode:\n"++ show (urlEncode False (urlDecode False working1))
  lift$ putStrLn$ " [codespeed] working2:\n"++ show working2
  lift$ putStrLn$ " [codespeed] failed3:\n"++ show failed3
  lift$ putStrLn$ " [codespeed] working1 decoded \n"++ BS.unpack (urlDecode False working1)
  lift$ putStrLn$ " [codespeed] working2 decoded \n"++ BS.unpack (urlDecode False working2)
  lift$ putStrLn$ " [codespeed] Bod computed \n"++BS.unpack bod
  lift$ putStrLn$ " [codespeed] Bod decoded \n"++BS.unpack (urlDecode False bod)
  lift$ putStrLn$ " [codespeed] Bod computed from: \n"++ B.unpack(renderJSONResult br)
  lift$ putStrLn$ " [codespeed] Submitting HTTP Post request: \n"++show req
  resp <- lift$ simpleHTTP req
  case resp of 
    Left err -> lift$ putStrLn$ " [codespeed] ERROR uploading: \n"++show err
    Right x  -> lift$ putStrLn$ " [codespeed] Got response from server:\n"++show x
#else
-- Version that uses http-conduit pkg:
  let bod = B.toStrict $ renderJSONResult br
  lift $ runResourceT $ do 
    req0    <- liftIO$ parseUrl addURL
    let 
        req1 = req0 { method = "POST"
                    , secure = False
--                    , requestBody = RequestBodyBS bod
--                    , requestBody = renderQuery False [("json",Just bod)]
--                    , queryString = renderQuery False [("json",Just (B.toStrict bod))]

                      -- Hardcode a working string from python:
                      , requestBody = RequestBodyBS working1
                    }
--        req = setQueryString [("json",bod)] req1
--        req = urlEncodedBody [("json",bod)] req1
        req = req1
    lift$ putStrLn$ " [codespeed] Submitting HTTP Post request: \n"++show req
    manager <- liftIO$ newManager def
    res     <- httpLbs req manager
    -- We could do something with the result (ByteString), but as long
    -- as its not error, we don't care:
    let _bstr = responseBody res
    return ()
#endif

  return ()

setQueryString = undefined

-- A working query generated by save_multiple_results.py : 
working1 :: BS.ByteString
working1 = "json=%5B%7B%22project%22%3A+%22MyProject%22%2C+%22executable%22%3A+%22myexe+O3+48bits%22%2C+%22environment%22%3A+%22cutter%22%2C+%22benchmark%22%3A+%22float%22%2C+%22commitid%22%3A+%228%22%2C+%22result_value%22%3A+2500.0%2C+%22branch%22%3A+%22default%22%7D%5D"

{- Trying different libraries but none of them are giving the same
behavior as python's urllib.urlencode...  To start, it leaves the =
in "json=".  For example: 

    >>> urllib.urlencode({ "foo": { "bar": 4, "baz": "st" }})
    'foo=%7B%27baz%27%3A+%27st%27%2C+%27bar%27%3A+4%7D'

Which is:

    "foo={'baz': 'st', 'bar': 4}"



    renderQuery False [("foo", Just "bar")]

 -}

renderJSONResult :: BenchmarkResult -> B.ByteString
renderJSONResult _ = 
  -- Wrap on the boilerplate:
  B.pack $ encodeStrict $ JSArray [bench1]
--    JSObject$ toJSObject [("json", JSArray [bench1])]
 where 
  bench1 = JSObject $ toJSObject $ 
   [ ("project",     s "Accelerate")
   , ("executable",  s "myexe 04 32bits")
   , ("benchmark",   s "float")
   , ("commitid",    s "14")
   , ("environment", s "cutter")
   , ("result_value", d 3.8)
   , ("branch",      s "default")

-- json=[{"project": "MyProject", "executable": "myexe O3 48bits", "environment": "cutter", "benchmark": "float", "commitid": "8", "result_value": 2500.0, "branch": "default"}]

   -- Plus add optional fields:
--   , ("revision_date", s "")  -- Optional. Default is taken either
--                            -- from VCS integration or from current date
--   , ("result_date", s "")    -- Optional, default is current date
--   , ("std_dev", showJSON (1.11111 :: Double))  -- Optional. Default is blank
--   , ("max", d 4001.6)  -- Optional. Default is blank
--   , ("min", d 3995.1)  -- Optional. Default is blank    
   -- RRN: Question: are max and min the observed max and min presumably?
   ]

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

