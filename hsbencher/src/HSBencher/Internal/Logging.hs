{-# LANGUAGE NamedFieldPuns #-}

module HSBencher.Internal.Logging 
       (LogDest(..), log, logOn, 
        logT, hsbencher_tag, chatter) where 

import qualified Data.ByteString.Char8 as B
import Control.Monad.Reader (ask, liftIO)
import qualified System.IO.Streams as Strm

import HSBencher.Types (Config(..), BenchM)
import Prelude hiding (log)

--------------------------------------------------------------------------------

-- | There are three logging destinations we care about.  The .dat
--   file, the .log file, and the user's screen (i.e. the user who
--   launched the benchmarks).
data LogDest = ResultsFile | LogFile | StdOut deriving Show

-- | Print a message (line) both to stdout and logFile:
log :: String -> BenchM ()
log = logOn [LogFile,StdOut] -- The commonly used default. 

-- | Log a line to a particular file and also echo to stdout.
logOn :: [LogDest] -> String -> BenchM ()
logOn modes s = do
  let bstr = B.pack s -- FIXME
  Config{logOut, resultsOut, stdOut} <- ask
  let go ResultsFile = Strm.write (Just bstr) resultsOut 
      go LogFile     = Strm.write (Just bstr) logOut     
      go StdOut      = Strm.write (Just bstr) stdOut
  liftIO$ mapM_ go modes


-- | Shorthand for tagged version of logging.
logT :: String -> BenchM ()
logT str = log$hsbencher_tag++str

-- | Logging straight to stdout (but with the hsbencher tag).
chatter :: String -> IO ()
chatter s = putStrLn $ hsbencher_tag ++ s

-- | The tag for printing hsbencher messagse
hsbencher_tag :: String
hsbencher_tag = " [hsbencher] "
