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
import System.Environment (getArgs, getEnv, getEnvironment, getExecutablePath)
import System.IO (Handle, hPutStrLn, stderr, openFile, hClose, hGetContents, hIsEOF, hGetLine,
                  IOMode(..), BufferMode(..), hSetBuffering)
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf
import Prelude hiding (log)

import HSBencher.Types hiding (env)
import HSBencher.Logging
import HSBencher.MeasureProcess

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

-- | Parse a simple "benchlist.txt" file.
parseBenchList :: String -> [Benchmark]
parseBenchList str = 
  map parseBench $                 -- separate operator, operands
  filter (not . null) $            -- discard empty lines
  map words $ 
  filter (not . isPrefixOf "#") $  -- filter comments
  map trim $
  lines str

-- Parse one line of a benchmark file (a single benchmark name with args).
parseBench :: [String] -> Benchmark
parseBench (h:m:tl) = Benchmark {name=h, compatScheds=expandMode m, args=tl }
parseBench ls = error$ "entry in benchlist does not have enough fields (name mode args): "++ unwords ls

strBool :: String -> Bool
strBool ""  = False
strBool "0" = False
strBool "1" = True
strBool  x  = error$ "Invalid boolean setting for environment variable: "++x

fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c

-- Compute a cut-down version of a benchmark's args list that will do
-- a short (quick) run.  The way this works is that benchmarks are
-- expected to run and do something quick if they are invoked with no
-- arguments.  (A proper benchmarking run, therefore, requires larger
-- numeric arguments be supplied.)
-- 
-- HOWEVER: there's a further hack here which is that leading
-- non-numeric arguments are considered qualitative (e.g. "monad" vs
-- "sparks") rather than quantitative and are not pruned by this
-- function.
shortArgs :: [String] -> [String]
shortArgs [] = []
-- Crop as soon as we see something that is a number:
shortArgs (h:tl) | isNumber h = []
		 | otherwise  = h : shortArgs tl

isNumber :: String -> Bool
isNumber s =
  case reads s :: [(Double, String)] of 
    [(n,"")] -> True
    _        -> False

-- Based on a benchmark configuration, come up with a unique suffix to
-- distinguish the executable.
uniqueSuffix :: BenchRun -> String
uniqueSuffix BenchRun{threads,sched,bench} =    
  "_" ++ show sched ++ 
   if threads == 0 then "_serial"
                   else "_threaded"


-- Indent for prettier output
indent :: [String] -> [String]
indent = map ("    "++)


--------------------------------------------------------------------------------
-- TODO -- all this Mode selection should be factored out to make this
-- benchmark script a bit more generic.
--------------------------------------------------------------------------------


-- [2012.05.03] RRN: ContFree is not exposed, thus removing it from the
-- default set, though you can still ask for it explicitly:
defaultSchedSet :: Set.Set Sched
defaultSchedSet = Set.difference (Set.fromList [minBound ..])
                               (Set.fromList [ContFree, NUMA, SMP])

-- Omitting ContFree, as it takes way too long for most trials
ivarScheds :: [Sched]
ivarScheds = [Trace, Direct, SMP, NUMA] 
-- ivarScheds = [Trace, Direct]

-- TODO -- we really need to factor this out into a configuration file.
schedToModule :: Sched -> String
schedToModule s = 
  case s of 
--   Trace    -> "Control.Monad.Par"
   Trace    -> "Control.Monad.Par.Scheds.Trace"
   Direct   -> "Control.Monad.Par.Scheds.Direct"
   ContFree -> "Control.Monad.Par.Scheds.ContFree"
   Sparks   -> "Control.Monad.Par.Scheds.Sparks"
   SMP      -> "Control.Monad.Par.Meta.SMP"
   NUMA     -> "Control.Monad.Par.Meta.NUMAOnly"
   None     -> "qualified Control.Monad.Par as NotUsed"

schedToCabalFlag :: Sched -> String
schedToCabalFlag s =
  case s of
    Trace -> "--flags=\"-ftrace\""
    Direct -> "--flags=\"-fdirect\""
    ContFree -> "--flags=\"-fcontfree\""
    Sparks -> "--flags=\"-fsparks\""
    SMP -> "--flags=\"-fmeta-smp\""
    NUMA -> "--flags=\"-fmeta-numa\""
    None -> ""

-- TODO - GET RID OF THIS:
-- | Expand the mode string into a list of specific schedulers to run:
expandMode :: String -> [Sched]
expandMode "default" = [Trace]
expandMode "none"    = [None]
-- TODO: Add RNG:
expandMode "futures" = [Sparks] ++ ivarScheds
expandMode "ivars"   = ivarScheds 
expandMode "chans"   = [] -- Not working yet!

-- [future] Schedulers in which nested execution WORKS!
expandMode "nested"      = [Sparks,Direct] -- [2012.11.26]
expandMode "nested+ivar" = [Direct]        -- [2012.11.26]

-- Also allowing the specification of a specific scheduler:
expandMode "Trace"    = [Trace]
expandMode "Sparks"   = [Sparks]
expandMode "Direct"   = [Direct]
expandMode "ContFree" = [ContFree]
expandMode "SMP"      = [SMP]
expandMode "NUMA"     = [NUMA]

expandMode s = error$ "Unknown Scheduler or mode: " ++s

----------------------------------------------------------------------------------------------------


runIgnoreErr :: String -> IO String
runIgnoreErr cm = 
  do lns <- runLines cm
     return (unlines lns)

-- | Create a thread that echos the contents of stdout/stderr InputStreams (lines) to
-- the appropriate places (as designated by the logging facility).
echoStream :: Bool -> Strm.InputStream B.ByteString -> BenchM (MVar ())
echoStream echoStdout outS = do
  conf <- ask
  mv   <- lift$ newEmptyMVar
  lift$ void$ forkIOH "echoStream thread"  $ 
    runReaderT (echoloop mv) conf 
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
runLogged :: String -> String -> BenchM (RunResult, [B.ByteString])
runLogged tag cmd = do 
  log$ " * Executing command: " ++ cmd
  SubProcess {wait,process_out,process_err} <-
    lift$ measureProcess
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
  res   <- lift$ wait
  log$ " * Command completed with "++show(length lines)++" lines of output." -- ++show res
  return (res,lines)

-- | Runs a command through the OS shell and returns stdout split into
-- lines.  (Ignore exit code.)
runLines :: String -> IO [String]
runLines cmd = do
  putStr$ "   * Executing: " ++ cmd 
  (Nothing, Just outH, Nothing, ph) <- createProcess 
     CreateProcess {
       cmdspec = ShellCommand cmd,
       env = Nothing,
       std_in  = Inherit,
       std_out = CreatePipe,
       std_err = Inherit,
       cwd = Nothing,
       close_fds = False,
       create_group = False
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
  Config{ghc_flags, ghc_RTS} <- ask
  let report = log$ printf " #      Return code %d Params: %s, RTS %s " (143::Int) ghc_flags ghc_RTS
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
