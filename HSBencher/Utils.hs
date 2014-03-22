{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

-- | Misc Small Helpers

module HSBencher.Utils where

import Control.Concurrent
import Control.Exception (evaluate, handle, SomeException, throwTo, fromException, AsyncException(ThreadKilled))
import qualified Data.Set as Set
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.IORef
import qualified Data.ByteString.Char8 as B
import Control.Monad.Reader -- (lift, runReaderT, ask)
import qualified System.IO.Streams as Strm
import qualified System.IO.Streams.Concurrent as Strm

import System.Process (system, waitForProcess, getProcessExitCode, runInteractiveCommand, 
                       createProcess, CreateProcess(..), CmdSpec(..), StdStream(..), readProcess)
import System.Environment (getArgs, getEnv, getEnvironment)
import System.IO (Handle, hPutStrLn, stderr, openFile, hClose, hGetContents, hIsEOF, hGetLine,
                  IOMode(..), BufferMode(..), hSetBuffering)
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath (dropTrailingPathSeparator, takeBaseName)
import System.Directory
import Text.Printf
import Prelude hiding (log)

import HSBencher.Types 
import HSBencher.Logging
import HSBencher.MeasureProcess

import Debug.Trace

----------------------------------------------------------------------------------------------------
-- Global constants, variables:

-- TODO: grab this from the command line arguments:
my_name :: String
my_name = "hsbencher"

-- | In seconds.
defaultTimeout :: Double
defaultTimeout = 150

-- | Global variable holding the main thread id.
main_threadid :: IORef ThreadId
main_threadid = unsafePerformIO$ newIORef (error "main_threadid uninitialized")

--------------------------------------------------------------------------------

-- These int list arguments are provided in a space-separated form:
parseIntList :: String -> [Int]
parseIntList = map read . words 

-- Remove whitespace from both ends of a string:
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- -- | Parse a simple "benchlist.txt" file.
-- parseBenchList :: String -> [Benchmark]
-- parseBenchList str = 
--   map parseBench $                 -- separate operator, operands
--   filter (not . null) $            -- discard empty lines
--   map words $ 
--   filter (not . isPrefixOf "#") $  -- filter comments
--   map trim $
--   lines str

-- Parse one line of a benchmark file (a single benchmark name with args).
-- parseBench :: [String] -> Benchmark
-- parseBench (h:m:tl) = Benchmark {name=h, compatScheds=expandMode m, args=tl }
-- parseBench ls = error$ "entry in benchlist does not have enough fields (name mode args): "++ unwords ls

strBool :: String -> Bool
strBool ""  = False
strBool "0" = False
strBool "1" = True
strBool  x  = error$ "Invalid boolean setting for environment variable: "++x

fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c

isNumber :: String -> Bool
isNumber s =
  case reads s :: [(Double, String)] of 
    [(n,"")] -> True
    _        -> False

-- Indent for prettier output
indent :: [String] -> [String]
indent = map ("    "++)

--------------------------------------------------------------------------------

runIgnoreErr :: String -> IO String
runIgnoreErr cm = 
  do lns <- runLines cm
     return (unlines lns)

-- | Create a thread that echos the contents of stdout/stderr InputStreams (lines) to
-- the appropriate places (as designated by the logging facility).
-- Returns an MVar used to synchronize on the completion of the echo thread.
echoStream :: Bool -> Strm.InputStream B.ByteString -> BenchM (MVar ())
echoStream echoStdout outS = do
  conf <- ask
  mv   <- lift$ newEmptyMVar  
  lift$ void$ forkIO $
      -- Make sure we get around to putting the MVar if something goes wrong:  
      handle (\ (exn::SomeException) -> do
                 hPutStrLn stderr $ " [hsbencher] Ignoring exception on echo thread: "++show exn
                 putMVar mv ())
             (runReaderT (echoloop mv) conf)
  return mv
 where
   echoloop mv = 
     do
        x <- lift$ Strm.read outS
        case x of
          Nothing -> lift$ putMVar mv ()
          Just ln -> do
            logOn (if echoStdout then [LogFile, StdOut] else [LogFile]) (B.unpack ln)
--            lift$ B.putStrLn ln
            echoloop mv

-- | Run a command and wait for all output.  Log output to the appropriate places.
--   The first argument is a "tag" to append to each output line to make things
--   clearer.
-- 
--   (Note: the command being run does use line harvesters, but had better not have
--   more than one RunResult.)
runLogged :: String -> String -> BenchM (RunResult, [B.ByteString])
runLogged tag cmd = do 
  log$ " * Executing command: " ++ cmd
  Config{ harvesters } <- ask
  SubProcess {wait,process_out,process_err} <-
    lift$ measureProcess harvesters
            CommandDescr{ command=ShellCommand cmd, envVars=[], timeout=Just 150, workingDir=Nothing }
  err2 <- lift$ Strm.map (B.append (B.pack "[stderr] ")) process_err
  both <- lift$ Strm.concurrentMerge [process_out, err2]
  both' <- lift$ Strm.map (B.append$ B.pack tag) both
  -- Synchronous: gobble up and echo all the input:
  let loop acc = do
        x <- lift$ Strm.read both'
        case x of
          Nothing -> return (reverse acc)
          Just ln -> do log (B.unpack ln)
                        loop (ln:acc)
  lines <- loop []
  (res,[]) <- lift$ wait
  log$ " * Command completed with "++show(length lines)++" lines of output." -- ++show res
  return (res,lines)

-- | Runs a command through the OS shell and returns stdout split into
-- lines.  (Ignore exit code and stderr.)
runLines :: String -> IO [String]
runLines cmd = do
  putStr$ "   * Executing: " ++ cmd 
  (Nothing, Just outH, Just _, ph) <- createProcess 
     CreateProcess {
       cmdspec = ShellCommand cmd,
       env = Nothing,
       std_in  = Inherit,
       std_out = CreatePipe,
       std_err = CreatePipe,
       cwd = Nothing,
       close_fds = False,
       create_group = False,
       delegate_ctlc = False
     }
  waitForProcess ph  
  Just _code <- getProcessExitCode ph  
  str <- hGetContents outH
  let lns = lines str
  putStrLn$ " -->   "++show (length lns)++" line(s)"
  return (lines str)

-- | Runs a command through the OS shell and returns the first line of
-- output.
runSL :: String -> IO String
runSL cmd = do
  lns <- runLines cmd
  case lns of
    h:_ -> return h
    []  -> error$ "runSL: expected at least one line of output for command "++cmd



-- Check the return code from a call to a test executable:
check :: Bool -> ExitCode -> String -> BenchM Bool
check _ ExitSuccess _           = return True
check keepgoing (ExitFailure code) msg  = do
  let report = log$ printf " #      Return code %d " (143::Int)
  case code of 
   143 -> 
     do report
        log         " #      Process TIMED OUT!!" 
   _ -> 
     do log$ " # "++msg 
	report 
        log "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        unless keepgoing $ 
          lift$ exitWith (ExitFailure code)
  return False


-- | Fork a thread but ALSO set up an error handler.
forkIOH :: String -> IO () -> IO ThreadId
forkIOH who action = 
  forkIO $ handle (\ (e::SomeException) -> 
                   case fromException e of
                     Just ThreadKilled -> return ()
                     Nothing -> do
                        printf $ "ERROR: "++who++": Got exception inside forked thread: "++show e++"\n"                       
			tid <- readIORef main_threadid
			throwTo tid e
		  )
           action



getCPULoad :: IO (Maybe Double)
getCPULoad = do
   cmd <- fmap trim $ runSL "which mpstat"
   fmap loop $ runLines cmd
 where
   -- The line after the line with %idle shoud have matching entries, for example:
   -- 10:18:05     CPU    %usr   %nice    %sys %iowait    %irq   %soft  %steal  %guest   %idle
   -- 10:18:05     all    0.06    0.00    0.06    0.19    0.00    0.00    0.00    0.00   99.69
   loop []  = Nothing
   loop [_] = Nothing
   loop (ln:nxt:tl)
     | "%idle" `elem` words ln = parseLine ln nxt
     | otherwise               = loop (nxt:tl)
   parseLine ln nxt =
     let w1 = words ln
         w2 = words nxt
     in if length w1 /= length w2
        then Nothing
        else case lookup "%idle" (zip w1 w2) of
               Nothing -> Nothing
               Just num ->
                 case reads num of
                   (n,_):_ -> Just (100 - n)
                   _       -> Nothing


  -- This is very fragile: 
  -- "mpstat | grep -A 5 \"%idle\" | tail -n 1 | xargs -n1 echo | tail -n 1 | awk -F \" \" '{print 100 - $1}'"


-- | A more persistent version of `takeBaseName`.
fetchBaseName :: FilePath -> FilePath
fetchBaseName path =
  takeBaseName $ dropTrailingPathSeparator path
  -- trybase  = takeBaseName (target bench)
  --            if trybase == ""
  --            then takeBaseName (takeDirectory (target bench))
  --            else trybase


-- | Create a backup copy of existing results_HOST.dat files.
backupResults :: String -> String -> IO ()
backupResults resultsFile logFile = do 
  e    <- doesFileExist resultsFile
  date <- runSL "date +%Y%m%d_%s"
  when e $ do
    renameFile resultsFile (resultsFile ++"."++date++".bak")
  e2   <- doesFileExist logFile
  when e2 $ do
    renameFile logFile     (logFile     ++"."++date++".bak")
