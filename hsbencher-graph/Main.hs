{-# LANGUAGE DeriveDataTypeable #-} 

module Main where

import System.Environment (getArgs, getEnv, getEnvironment)
import System.Console.GetOpt (getOpt', ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.IO (Handle, hPutStrLn, stderr, openFile, hClose, hGetContents, hIsEOF, hGetLine,
                  IOMode(..), BufferMode(..), hSetBuffering)

-- import qualified System.IO.Streams as Strm
-- import qualified System.IO.Streams.Concurrent as Strm
-- import qualified System.IO.Streams.Process as Strm
-- import qualified System.IO.Streams.Combinators as Strm

import Data.List (isInfixOf, intersperse)
-- import Data.List.Split (splitOn)
-- import Data.String.Utils (strip)

import Control.Monad (unless,when)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn)


-- Exceptions
import Control.Exception
import Data.Typeable



import qualified Prelude as P
import Prelude hiding (init) 
---------------------------------------------------------------------------
--

{- DEVLOG
-} 




---------------------------------------------------------------------------
-- //                                                                 \\ --
---------------------------------------------------------------------------

-- | Command line flags to the benchmarking executable.
data Flag = ShowHelp | ShowVersion
          | File String
          | DataIn String  -- Readable as "Orientation" 
  deriving (Eq,Ord,Show,Read)

data Orientation = Rows | Columns
                 deriving (Eq, Ord, Show, Read ) 

-- | Exceptions that may occur
data Error
  = FlagsNotValidE String
    deriving (Show, Typeable) 

instance Exception Error 

-- | Command line options.
core_cli_options :: [OptDescr Flag]
core_cli_options = 
     [ Option ['h'] ["help"] (NoArg ShowHelp)
        "Show this help message and exit."
     , Option ['f'] ["file"] (ReqArg File "FileName.csv")    "Use a CSV file as input"
     , Option ['d'] ["data"] (ReqArg DataIn "Rows/Columns")  "Data series are along a row/column" 
     ]

-- | Multiple lines of usage info help docs.
fullUsageInfo :: String
fullUsageInfo = usageInfo docs core_cli_options
 where 
  docs = "USAGE: grapher <flags> ...\n"++
         "\n\nhsbencher-graph general options: \n"
--   ++ generalUsageStr

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


  ---------------------------------------------------------------------------
  -- Perform the task specified by the command line args

  hPutStrLn stderr "Hello World" 

  hPutStrLn stderr $ show options
  -- catch (
  --   case mode of
  --     Download -> download options 
  --     Upload   -> upload options 
  --   ) (\e ->
  --       case e of
  --         FlagsNotValidE str -> putStrLn $ "Caught error: "++ str
  --     )



