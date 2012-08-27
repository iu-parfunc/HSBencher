{-# LANGUAGE BangPatterns, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleContexts #-}
--------------------------------------------------------------------------------
-- NOTE: This is best when compiled with "ghc -threaded"
-- However, ideally for real benchmarking runs we WANT the waitForProcess below block the whole process.
-- However^2, currently [2012.05.03] when running without threads I get errors like this:
--   benchmark.run: bench_hive.log: openFile: resource busy (file is locked)

-- NOTE: Under 7.2 I'm running into this HSH problem:
-- 
-- benchmark.hs: HSH/Command.hs:(289,14)-(295,45): Missing field in record construction System.Process.Internals.create_group
--------------------------------------------------------------------------------


{- |
   
benchmark.hs

This program runs a set of benchmarks contained in the current
directory.  It produces two files as output:

    results_HOSTNAME.dat
    bench_HOSTNAME.log

---------------------------------------------------------------------------   
            ASSUMPTIONS -- about directory and file organization
---------------------------------------------------------------------------   



---------------------------------------------------------------------------
                                 USAGE
---------------------------------------------------------------------------

  Usage: [set environment vars] ./benchmark.hs [--no-recomp, --par]

   Call it with the following environment variables...

     SHORTRUN=1 to get a shorter run for testing rather than benchmarking.

     THREADS="1 2 4" to run with # threads = 1, 2, or 4.

     KEEPGOING=1 to keep going after the first error.

     TRIALS=N to control the number of times each benchmark is run.

     BENCHLIST=foo.txt to select the benchmarks and their arguments
		       (uses benchlist.txt by default)

     SCHEDS="Trace Direct Sparks" -- Restricts to a subset of schedulers.

     GENERIC=1 to go through the generic (type class) monad par
               interface instead of using each scheduler directly
 
     ENVS='[[("KEY1", "VALUE1")], [("KEY1", "VALUE2")]]' to set different 
     configurations of environment variables to be set *at runtime*. Useful 
     for NUMA_TOPOLOGY, for example.

   Additionally, this script will propagate any flags placed in the
   environment variables $GHC_FLAGS and $GHC_RTS.  It will also use
   $GHC, if available, to select the $GHC executable.

   
---------------------------------------------------------------------------
                                << TODO >>
   ---------------------------------------------------------------------------

   * Replace environment variable argument passing with proper flags/getopt.

   <Things that worked at one time but need to be cleaned up:>
     
     * Further enable packing up a benchmark set to run on a machine
       without GHC (as with Haskell Cnc)
     
     * Clusterbench -- adding an additional layer of parameter variation.

-}

module Main (main) where 

import qualified HSH 
import HSH ((-|-))
import Prelude hiding (log)
import Control.Applicative    
import Control.Concurrent
import Control.Concurrent.Chan
import GHC.Conc (numCapabilities)
import Control.Monad.Reader
import Control.Exception (evaluate, handle, SomeException, throwTo, fromException, AsyncException(ThreadKilled))
import Debug.Trace
import Data.Char (isSpace)
import Data.Maybe (isJust, fromJust)
import Data.Word (Word64)
import Data.IORef
import Data.List (intercalate)
import qualified Data.Set as S
import Data.List (isPrefixOf, tails, isInfixOf, delete)
import System.Console.GetOpt (getOpt, ArgOrder(Permute), OptDescr(Option), ArgDescr(NoArg), usageInfo)
import System.Environment (getArgs, getEnv, getEnvironment)
import System.Directory
import System.Posix.Env (setEnv)
import System.Random (randomIO)
import System.Exit
import System.FilePath (splitFileName, (</>))
import System.Process (system, waitForProcess, getProcessExitCode, 
                       createProcess, CreateProcess(..), CmdSpec(..), StdStream(..))
import System.IO (Handle, hPutStrLn, stderr, openFile, hClose, hGetContents, hIsEOF, hGetLine, IOMode(..))
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf
----------------------------------------------------------------------------------------------------


-- The global configuration for benchmarking:
data Config = Config 
 { benchlist      :: [Benchmark]
 , benchversion   :: (String, Double) -- benchlist file name and version number (e.g. X.Y)
 , threadsettings :: [Int]  -- A list of #threads to test.  0 signifies non-threaded mode.
 , maxthreads     :: Int
 , trials         :: Int    -- number of runs of each configuration
 , shortrun       :: Bool
 , keepgoing      :: Bool   -- keep going after error
 , ghc            :: String -- ghc compiler path
 , ghc_pkg        :: String
 , ghc_flags      :: String
 , ghc_RTS        :: String -- +RTS flags
 , scheds         :: S.Set Sched -- subset of schedulers to test.
 , hostname       :: String 
 , resultsFile    :: String -- Where to put timing results.
 , logFile        :: String -- Where to put more verbose testing output.
 
   -- outHandles: Logging can be dynamically redirected away from the
   -- filenames (logFile, resultsFile) and towards specific Handles:
 , outHandles     :: Maybe (Buffer String, 
			    Buffer String,
			    Buffer String)
   -- A set of environment variable configurations to test
 , envs           :: [[(String, String)]]
 }


-- Represents a configuration of an individual run.
--  (number of
-- threads, other flags, etc):
data BenchRun = BenchRun
 { threads :: Int
 , sched   :: Sched 
 , bench   :: Benchmark
 , env     :: [(String, String)] -- ADDITIONAL bindings for the environment
 } deriving (Eq, Show, Ord)

data Benchmark = Benchmark
 { name :: String
 , compatScheds :: [Sched]
 , args :: [String]
 } deriving (Eq, Show, Ord)

-- | Name of a script to time N runs of a program:
--   (I used a haskell script for this but ran into problems at one point):
--
-- CONTRACT FOR NTIMES:
--   * Return ONLY a series of three times (min/med/max) on a single line of stdout
--   * Optionally return matching productivities after the min/med/max times on the same line.
--   * Return the original output of the program, or any additional
--     output, on stderr.
ntimes = "./ntimes_minmedmax"

gc_stats_flag = " -s " 
-- gc_stats_flag = " --machine-readable -t "

exedir = "./bin"

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- Retrieve the configuration from the environment.
getConfig = do
  hostname <- HSH.runSL$ "hostname -s"
  env      <- getEnvironment

  let get v x = case lookup v env of 
		  Nothing -> x
		  Just  s -> s

      bench = get "BENCHLIST" "benchlist.txt"
      logFile = "bench_" ++ hostname ++ ".log"
      shortrun = strBool (get "SHORTRUN"  "0")
  -- We can't use numCapabilities as the number of hardware threads
  -- because this script may not be running in threaded mode.

  let scheds = case get "SCHEDS" "" of 
		"" -> defaultSchedSet
		s  -> S.fromList (map read (words s))

  case get "GENERIC" "" of 
    "" -> return ()
    s  -> error$ "GENERIC env variable not handled yet.  Set to: " ++ show s

  maxthreads <- getNumberOfCores
  benchstr   <- readFile bench
  let ver = case filter (isInfixOf "ersion") (lines benchstr) of 
	      (h:t) -> read $ head $ filter isNumber (words h)
	      []    -> 0
      conf = Config 
           { hostname, logFile, scheds, shortrun    
	   , ghc        =       get "GHC"       "ghc"
           , ghc_pkg    =       get "GHC_PKG"   "ghc-pkg"
	   , ghc_RTS    =       get "GHC_RTS"   ("-qa " ++ gc_stats_flag) -- Default RTS flags.
  	   , ghc_flags  = (get "GHC_FLAGS" (if shortrun then "" else "-O2")) 
	                  ++ " -rtsopts" -- Always turn on rts opts.
	   , trials         = read$ get "TRIALS"    "1"
	   , benchlist      = parseBenchList benchstr
	   , benchversion   = (bench, ver)
	   , maxthreads     = maxthreads
	   , threadsettings = parseIntList$ get "THREADS" (show maxthreads)
	   , keepgoing      = strBool (get "KEEPGOING" "0")
	   , resultsFile    = "results_" ++ hostname ++ ".dat"
	   , outHandles     = Nothing
           , envs           = read $ get "ENVS" "[[]]"
	   }

  runReaderT (log$ "Read list of benchmarks/parameters from: "++bench) conf

  -- Here are the DEFAULT VALUES:
  return conf

-- TODO: Windows!
getNumberOfCores :: IO Int
getNumberOfCores = do 
  -- Determine the number of cores.
  d <- doesDirectoryExist "/sys/devices/system/cpu/"
  uname <- HSH.runSL "uname"
  str :: String 
       <- if d 
	  then HSH.runSL$ "ls  /sys/devices/system/cpu/" -|- HSH.egrep "cpu[0123456789]*$" -|- HSH.wcL
	  else if uname == "Darwin"
	  then HSH.runSL$ "sysctl -n hw.ncpu"
	  else error$ "Don't know how to determine the number of threads on platform: "++ show uname
  -- Note -- how do we find the # of threads ignoring hyperthreading?
  return (read str)


-- | Remove RTS options that are specific to -threaded mode.
pruneThreadedOpts :: [String] -> [String]
pruneThreadedOpts = filter (`notElem` ["-qa", "-qb"])

--------------------------------------------------------------------------------
-- TODO -- all this Mode selection should be factored out to make this
-- benchmark script a bit more generic.
--------------------------------------------------------------------------------

data Sched 
   = Trace | Direct | Sparks | ContFree | SMP | NUMA
   | None
 deriving (Eq, Show, Read, Ord, Enum, Bounded)

-- [2012.05.03] RRN: ContFree is not exposed, so removing it from the
-- default set, though you can still ask for it explicitly:
defaultSchedSet = S.difference (S.fromList [minBound ..])
                               (S.fromList [ContFree, NUMA])

-- | Expand the mode string into a list of specific schedulers to run:
expandMode :: String -> [Sched]
expandMode "default" = [Trace]
expandMode "none"    = [None]
-- TODO: Add RNG:
expandMode "futures" = [Sparks] ++ ivarScheds
expandMode "ivars"   = ivarScheds 
expandMode "chans"   = [] -- Not working yet!

-- Also allowing the specification of a specific scheduler:
expandMode "Trace"    = [Trace]
expandMode "Sparks"   = [Sparks]
expandMode "Direct"   = [Direct]
expandMode "ContFree" = [ContFree]
expandMode "SMP"      = [SMP]
expandMode "NUMA"     = [NUMA]

expandMode s = error$ "Unknown Scheduler or mode: " ++s

-- Omitting ContFree, as it takes way too long for most trials
ivarScheds = [Trace, Direct, SMP, NUMA] 
-- ivarScheds = [Trace, Direct]

-- TODO -- we really need to factor this out into a configuration file.
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

schedToCabalFlag s =
  case s of
    Trace -> "-ftrace"
    Direct -> "-fdirect"
    ContFree -> "-fcontfree"
    Sparks -> "-fsparks"
    SMP -> "-fmeta-smp"
    NUMA -> "-fmeta-numa"
    None -> ""
  

--------------------------------------------------------------------------------
-- Misc Small Helpers
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
parseBench (h:m:tl) = Benchmark {name=h, compatScheds=expandMode m, args=tl }
parseBench ls = error$ "entry in benchlist does not have enough fields (name mode args): "++ unwords ls

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
shortArgs [] = []
-- Crop as soon as we see something that is a number:
shortArgs (h:tl) | isNumber h = []
		 | otherwise  = h : shortArgs tl

isNumber s =
  case reads s :: [(Double, String)] of 
    [(n,"")] -> True
    _        -> False

-- Based on a benchmark configuration, come up with a unique suffix to
-- distinguish the executable.
uniqueSuffix BenchRun{threads,sched,bench} =    
  "_" ++ show sched ++ 
   if threads == 0 then "_serial"
                   else "_threaded"

--------------------------------------------------------------------------------
-- Error handling
--------------------------------------------------------------------------------

-- Check the return code from a call to a test executable:
check :: Bool -> ExitCode -> String -> ReaderT Config IO Bool
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

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

-- | There are three logging destinations we care about.  The .dat
--   file, the .log file, and the user's screen (i.e. the user who
--   launched the benchmarks).
data LogDest = ResultsFile | LogFile | StdOut

-- | Print a message both to stdout and logFile:
log :: String -> ReaderT Config IO ()
log = logOn [LogFile,StdOut] -- The commonly used default. 

-- | Log to a particular file and also echo to stdout.
logOn :: [LogDest] -> String -> ReaderT Config IO ()
logOn modes s = do
  Config{outHandles,logFile,resultsFile} <- ask
  let tofile m = case m of 
	       ResultsFile -> resultsFile
	       LogFile     -> logFile
	       StdOut      -> "/dev/stdout"
      capped = s++"\n"
  case outHandles of 
    -- If outHandles are not set, direct logging info directly to files:
    Nothing -> lift$ 
                 forM_ (map tofile modes) 
		       (\ f -> HSH.appendTo f capped)
    Just (logBuffer,resultBuffer,stdoutBuffer) -> 
     forM_ modes $ \ mode -> 
       case mode of 
	ResultsFile -> lift$ writeBuffer resultBuffer s
	LogFile     -> lift$ writeBuffer logBuffer    s
	StdOut      -> lift$ writeBuffer stdoutBuffer s

-- | Create a backup copy of existing results_HOST.dat files.
backupResults Config{resultsFile, logFile} = do 
  e    <- doesFileExist resultsFile
  date <- HSH.runSL "date +%Y%m%d_%s"
  when e $ do
    renameFile resultsFile (resultsFile ++"."++date++".bak")
  e2   <- doesFileExist logFile
  when e2 $ do
    renameFile logFile     (logFile     ++"."++date++".bak")


path :: [FilePath] -> FilePath
path [] = ""
path ls = foldl1 (</>) ls

--------------------------------------------------------------------------------
-- Compiling Benchmarks
--------------------------------------------------------------------------------

-- | Invoke cabal for all of the schedulers in the current config
invokeCabal :: ReaderT Config IO Bool
invokeCabal = do
  Config{ghc, ghc_pkg, ghc_flags, shortrun, scheds} <- ask  
  bs <- forM (S.toList scheds) $ \sched -> do
          let schedflag = schedToCabalFlag sched
              cmd = unwords [ "cabal install"
                            , "--with-ghc=" ++ ghc
                            , "--with-ghc-pkg=" ++ ghc_pkg
                            , "--ghc-options='" ++ ghc_flags ++ "'"
                            , schedflag
                            , "--prefix=`pwd`"
--                            , "--symlink-bindir=" -- Causes problems on linux [2012.05.02] -RRN
                            , "--disable-documentation"
                            , "--program-suffix='_" ++ show sched ++ "_threaded.exe'"
--                            , "&& cabal build"
                            ]
          log$ "\nRunning cabal... "
          log$ "============================================================"
          (_,code) <- runCmdWithEnv True [] cmd
          check False code "ERROR, benchmark.hs: cabal-based compilation failed."          
  
  return $ all id bs


-- | Build a single benchmark in a single configuration WITHOUT cabal.
compileOne :: BenchRun -> (Int,Int) -> ReaderT Config IO Bool
compileOne br@(BenchRun { threads=numthreads
                        , sched
                        , bench=(Benchmark testPath _ args_)
                        }) 
	      (iterNum,totalIters) = 
  do Config{ghc, ghc_flags, shortrun} <- ask

     uid :: Word64 <- lift$ randomIO
     let flags_ = case numthreads of
		   0 -> ghc_flags
		   _ -> ghc_flags++" -threaded"
	 flags = flags_ ++ " -fforce-recomp -DPARSCHED=\""++ (schedToModule sched) ++ "\""         
	 (diroffset,testRoot) = splitFileName testPath
         
         -- TODO: make this command-line configurable.
--       srcBase = "./src/"
--       containingdir = srcBase </> diroffset
--	 hsfile = srcBase </> testPath++".hs"
         containingdir = diroffset
         hsfile = testPath++".hs"
	 suffix = uniqueSuffix br
         outdir = "build" </> testRoot++suffix++"_"++show uid
	 exefile = exedir </> testRoot ++ suffix ++ ".exe"
         args = if shortrun then shortArgs args_ else args_

     log$ "\n--------------------------------------------------------------------------------"
     log$ "  Compiling Config "++show iterNum++" of "++show totalIters++
	  ": "++testRoot++" (args \""++unwords args++"\") scheduler "++show sched ++ 
           if numthreads==0 then " serial" else " threaded"
     log$ "--------------------------------------------------------------------------------\n"

     log$"First, creating a directory for intermediate compiler files: "++outdir
     code1 <- lift$ system$ "mkdir -p "++outdir
     code2 <- lift$ system$ "mkdir -p "++exedir
     check False code1 "ERROR, benchmark.hs: making compiler temp dir failed."
     check False code2 "ERROR, benchmark.hs: making compiler temp dir failed."

     log$"Next figure out what kind of benchmark this is by poking around the file system: "
     log$"  Checking for: "++hsfile
     log$"  Checking for: "++containingdir</>"Makefile"

     e  <- lift$ doesFileExist hsfile
     d  <- lift$ doesDirectoryExist containingdir
     mf <- lift$ doesFileExist$     containingdir </> "Makefile"

     if e then do 
	 log "Compiling with a single GHC command: "
         -- HACK for pinning to threads: (TODO - should probably make this for NUMA)
         let pinobj = path ["..","dist","build","cbits","pin.o"]
         pinObjExists <- lift $ doesFileExist pinobj
	 let cmd = unwords [ ghc, "--make"
                           , if pinObjExists then pinobj else ""
                           , "-i"++containingdir
                           , "-outputdir "++outdir
                           , flags, hsfile, "-o "++exefile]

	 log$ "  "++cmd ++"\n"
         tmpfile <- mktmpfile
	 -- Having trouble getting the &> redirection working.  Need to specify bash specifically:
	 code <- lift$ system$ "bash -c "++show (cmd++" &> "++tmpfile)
	 flushtmp tmpfile 

	 check False code "ERROR, benchmark.hs: compilation failed."

     else if (d && mf && diroffset /= ".") then do 
 	log " ** Benchmark appears in a subdirectory with Makefile.  NOT supporting Makefile-building presently."
        error "No makefile-based builds supported..."
-- 	log " ** Benchmark appears in a subdirectory with Makefile.  Using it."
-- 	log " ** WARNING: Can't be sure to control compiler options for this benchmark!"
-- 	log " **          (Hopefully it will obey the GHC_FLAGS env var.)"
-- 	log$ " **          (Setting GHC_FLAGS="++ flags++")"

-- 	-- First we make clean because we can't trust the makefile to rebuild when flags change:
-- 	code1 <- lift$ run$  "(cd "++containingdir++" && make clean)" 
-- 	check code1 "ERROR, benchmark.hs: Benchmark's 'make clean' failed"

-- 	-- !!! TODO: Need to redirect output to log here:

-- 	code2 <- lift$ run$ setenv [("GHC_FLAGS",flags),("UNIQUE_SUFFIX",uniqueSuffix br)]
-- 		       ("(cd "++containingdir++" && make)")
-- 	check code2 "ERROR, benchmark.hs: Compilation via benchmark Makefile failed:"

     else do 
	log$ "ERROR, benchmark.hs: File does not exist: "++hsfile
	lift$ exitFailure

--------------------------------------------------------------------------------
-- Running Benchmarks
--------------------------------------------------------------------------------

-- If the benchmark has already been compiled doCompile=False can be
-- used to skip straight to the execution.
runOne :: BenchRun -> (Int,Int) -> ReaderT Config IO ()
runOne br@(BenchRun { threads=numthreads
                    , sched
                    , bench=(Benchmark testPath _ args_)
                    , env}) 
          (iterNum,totalIters) = do
  Config{..} <- ask
  let args = if shortrun then shortArgs args_ else args_
      (_,testRoot) = splitFileName testPath
  log$ "\n--------------------------------------------------------------------------------"
  log$ "  Running Config "++show iterNum++" of "++show totalIters++
       ": "++testRoot++" (args \""++unwords args++"\") scheduler "++show sched++"  threads "++show numthreads++" (Env="++show env++")"
  log$ "--------------------------------------------------------------------------------\n"
  pwd <- lift$ getCurrentDirectory
  log$ "(In directory "++ pwd ++")"

  log$ "Next run 'who', reporting users other than the current user.  This may help with detectivework."
--  whos <- lift$ run "who | awk '{ print $1 }' | grep -v $USER"
  whos <- lift$ HSH.run$ "who" -|- map (head . words)
  user <- lift$ getEnv "USER"

  log$ "Who_Output: "++ unwords (filter (/= user) whos)

  -- numthreads == 0 indicates a serial run:
  let 
      rts = gc_stats_flag ++" "++
            case numthreads of
	     0 -> unwords (pruneThreadedOpts (words ghc_RTS))
	     _ -> ghc_RTS  ++" -N"++show numthreads
      exefile = exedir </> testRoot ++ uniqueSuffix br ++ ".exe"
  ----------------------------------------
  -- Now Execute:
  ----------------------------------------
  -- If we failed compilation we don't bother running either:
  let ntimescmd = printf "%s %d %s %s +RTS %s -RTS" ntimes trials exefile (unwords args) rts

  -- One option woud be dynamic feedback where if the first one
  -- takes a long time we don't bother doing more trials.  

  (str,code) <- runCmdWithEnv (not shortrun) env ntimescmd

  let (ts,prods) = 
       case code of
	ExitSuccess     -> 
           case map words (lines str) of
	      -- Here we expect a very specific output format from ntimes:
	      ["REALTIME":ts, "PRODUCTIVITY":prods] -> (ts, prods)
	      _ -> error "bad output from ntimes, expected two timing numbers"     
	ExitFailure 143 -> (["TIMEOUT","TIMEOUT","TIMEOUT"],[])
	ExitFailure 15  -> (["TIMEOUT","TIMEOUT","TIMEOUT"],[])
	-- TEMP ^^ : [2012.01.16], ntimes is for some reason getting 15 instead of 143.  HACKING this temporarily ^
	ExitFailure _   -> (["ERR","ERR","ERR"],[])

      pads n s = take (max 1 (n - length s)) $ repeat ' '
      padl n x = pads n x ++ x 
      padr n x = x ++ pads n x
      
      -- These are really (time,prod) tuples, but a flat list of
      -- scalars is simpler and readable by gnuplot:
      formatted = (padl 15$ unwords ts) ++"   "++ unwords prods -- prods may be empty!

  log $ " >>> MIN/MEDIAN/MAX (TIME,PROD) " ++ formatted
  logOn [ResultsFile]$ 
    printf "%s %s %s %s %s" (padr 35 testRoot)   (padr 20$ intercalate "_" args)
	                    (padr 7$ show sched) (padr 3$ show numthreads) formatted

  return ()
  

-- Helpers for creating temporary files:
------------------------------------------------------------
mktmpfile = do 
   n :: Word64 <- lift$ randomIO
   return$ "._Temp_output_buffer_"++show (n)++".txt"
-- Flush the temporary file to the log file, returning its contents
-- and deleting it in the process:
flushtmp tmpfile = 
  do Config{shortrun} <- ask
     output <- lift$ readFile tmpfile
     mapM_ (logOn [LogFile,StdOut]) (indent$ lines output) 
     lift$ removeFile tmpfile
     return output
-- Indent for prettier output
indent = map ("    "++)
------------------------------------------------------------

-- Helper for launching processes with logging and error checking
-----------------------------------------------------------------
-- [2012.05.03] HSH has been causing no end of problems in the
-- subprocess-management department.  Here we instead use the
-- underlying createProcess library function:
runCmdWithEnv echo env cmd = do 
  -- This current design has the unfortunate disadvantage that it
  -- produces no observable output until the subprocess is FINISHED.
  log$ "Executing: " ++ cmd      
  baseEnv <- lift$ getEnvironment
  (Nothing, Just outH, Just errH, ph) <- lift$ createProcess 
     CreateProcess {
       cmdspec = ShellCommand cmd,
       env = Just$ baseEnv ++ env,
       std_in  = Inherit,
       std_out = CreatePipe,
       std_err = CreatePipe,
       -- [2012.05.03] Having problems with redirecting stderr for ntimes:
       -- Inheriting WORKS, CreatePipe WORKS, UseHandle DOESN'T:
       -- Redirecting to a file fails with several of these errors:
       --    ./ntimes: line 69: /dev/stderr: Not a directory
       cwd = Nothing,
       close_fds = False,
       create_group = False
     }
  
  mv1 <- echoThread echo outH
  mv2 <- echoThread echo errH
  lift$ waitForProcess ph  
  Just code <- lift$ getProcessExitCode ph  
  outStr <- lift$ takeMVar mv1
  _      <- lift$ takeMVar mv2
                
  Config{keepgoing} <- ask
  check keepgoing code ("ERROR, benchmark.hs: command \""++cmd++"\" failed with code "++ show code)
  return (outStr, code)

-----------------------------------------------------------------
runIgnoreErr :: String -> IO String
runIgnoreErr cm = 
  do (str,force) <- HSH.run cm
     (err::String, code::ExitCode) <- force
     return str
-----------------------------------------------------------------

-- | Create a thread that echos the contents of a Handle as it becomes
--   available.  Then return all text read through an MVar when the
--   handle runs dry.
echoThread :: Bool -> Handle -> ReaderT Config IO (MVar String)
echoThread echoStdout hndl = do
  mv   <- lift$ newEmptyMVar
  conf <- ask
  lift$ void$ forkIOH "echo thread" Nothing $ 
    runReaderT (echoloop mv []) conf    
  return mv  
 where
   echoloop mv acc = 
     do b <- lift$ hIsEOF hndl 
        if b then do lift$ hClose hndl
                     lift$ putMVar mv (unlines$ reverse acc)
         else do ln <- lift$ hGetLine hndl
                 logOn (if echoStdout then [LogFile, StdOut] else [LogFile]) ln 
                 echoloop mv (ln:acc)

-- Take a lazy string that performs blocking IO behind the scenes, echo it.
-- echoLazyIO :: String -> ReaderT Config IO (MVar )
-- echoLazyIO str = 
--   forM_ (lines str) $ \ln -> do 
--     return ""

--------------------------------------------------------------------------------

whichVariant "benchlist.txt"        = "desktop"
whichVariant "benchlist_server.txt" = "server"
whichVariant "benchlist_laptop.txt" = "laptop"
whichVariant _                      = "unknown"

resultsHeader :: Config -> IO ()
resultsHeader Config{ghc, trials, ghc_flags, ghc_RTS, maxthreads, resultsFile, logFile, benchversion, shortrun } = do
  let (benchfile, ver) = benchversion
  -- There has got to be a simpler way!
  -- branch   <- runIgnoreErr "git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'"
  -- branch <- "git symbolic-ref HEAD"
  branch   <- HSH.run "git name-rev --name-only HEAD"
  revision <- HSH.run "git rev-parse HEAD"
  -- Note that this will NOT be newline-terminated:
  hashes   <- HSH.run "git log --pretty=format:'%H'"
  mapM_ HSH.runIO $ concat $ 
   [
     e$ "# TestName Variant NumThreads   MinTime MedianTime MaxTime  Productivity1 Productivity2 Productivity3"
   , e$ "#    "        
   , e$ "# `date`"
   , e$ "# `uname -a`" 
   , e$ "# Ran by: `whoami` " 
   , e$ "# Determined machine to have "++show maxthreads++" hardware threads."
   , e$ "# `"++ghc++" -V`" 
   , e$ "# "                                                                
   , e$ "# Running each test for "++show trials++" trial(s)."
   , e$ "#  ... with compiler options: " ++ ghc_flags
   , e$ "#  ... with runtime options: " ++ ghc_RTS
   , e$ "# Benchmarks_File: " ++ benchfile
   , e$ "# Benchmarks_Variant: " ++ if shortrun then "SHORTRUN" else whichVariant benchfile
   , e$ "# Benchmarks_Version: " ++ show ver
   , e$ "# Git_Branch: " ++ trim branch
   , e$ "# Git_Hash: "   ++ trim revision
   , e$ "# Git_Depth: "  ++ show (length (lines hashes))
   , e$ "# Using the following settings from environment variables:" 
   , e$ "#  ENV BENCHLIST=$BENCHLIST"
   , e$ "#  ENV THREADS=   $THREADS"
   , e$ "#  ENV TRIALS=    $TRIALS"
   , e$ "#  ENV SHORTRUN=  $SHORTRUN"
   , e$ "#  ENV SCHEDS=    $SCHEDS"
   , e$ "#  ENV KEEPGOING= $KEEPGOING"
   , e$ "#  ENV GHC=       $GHC"
   , e$ "#  ENV GHC_FLAGS= $GHC_FLAGS"
   , e$ "#  ENV GHC_RTS=   $GHC_RTS"
   , e$ "#  ENV ENVS=      $ENVS"
   ]
 where 
   -- This regressed [2012.03.07] it is no longer writing to the logFile:
--    e s = ("echo \""++s++"\"") -|- HSH.tee ["/dev/stdout", logFile] -|- HSH.appendTo resultsFile
   e s = [("echo \""++s++"\"") -|- HSH.appendTo "/dev/stdout", 
	  ("echo \""++s++"\"") -|- HSH.appendTo logFile, 
	  ("echo \""++s++"\"") -|- HSH.appendTo resultsFile]


----------------------------------------------------------------------------------------------------
-- Main Script
----------------------------------------------------------------------------------------------------

-- | Command line flags.
data Flag = ParBench 
          | BinDir FilePath
          | NoRecomp | NoCabal | NoClean
  deriving Eq

-- | Command line options.
cli_options :: [OptDescr Flag]
cli_options = 
     [ Option [] ["par"] (NoArg ParBench) 
       "Build benchmarks in parallel."
     , Option [] ["no-recomp"] (NoArg NoRecomp)
       "Don't perform any compilation of benchmark executables.  Implies -no-clean."
     , Option [] ["no-clean"] (NoArg NoClean)
       "Do not clean pre-existing executables before beginning."
     , Option [] ["no-cabal"] (NoArg NoCabal)
       "Build directly through GHC even if .cabal file is present."
     
     -- , Option [] ["bindir"] (ReqArg BinDir)
     --   "Place or expect built binaries to be in BINDIR"
     ]

-- | Global variable holding the main thread id.
main_threadid :: IORef ThreadId
main_threadid = unsafePerformIO$ newIORef (error "main_threadid uninitialized")

main = do
  id <- myThreadId
  writeIORef main_threadid id

  cli_args <- getArgs
  let (options,args,errs) = getOpt Permute cli_options cli_args
  unless (null errs && null args) $ do
    putStrLn$ "Errors parsing command line options:" 
    mapM_ (putStr . ("   "++)) errs
    putStr$ usageInfo "\nUsage: [options]" cli_options
    exitFailure

  -- HACK: with all the inter-machine syncing and different version
  -- control systems I run into permissions problems sometimes:
  system "chmod +x ./ntime* ./*.sh"

  conf@Config{..} <- getConfig    

  hasMakefile <- doesFileExist "Makefile"
  (cabalFile,finish) <- HSH.run "ls *.cabal"
  (_::String,_::ExitCode)   <- finish  
  let hasCabalFile = (cabalFile /= "") &&
                     not (NoCabal `elem` options)

  runReaderT 
    (do         

        lift$ backupResults conf
	log "Writing header for result data file:"
	lift$ resultsHeader conf 
        let recomp  = not $ NoRecomp `elem` options
            doclean = (not $ NoCabal `elem` options) && recomp
        when doclean $ 
          let cleanit cmd = when (not $ NoClean `elem` options) $ do
                log$ "Before testing, first '"++ cmd ++"' for hygiene."
                code <- lift$ system$ cmd++" &> clean_output.tmp"
                check False code "ERROR: cleaning failed."
	        log " -> Cleaning Succeeded."
                liftIO$ removeFile "clean_output.tmp"
          in      if hasMakefile  then cleanit "make clean"
             else if hasCabalFile then cleanit "cabal clean"
             else    return ()

        unless recomp $ log "[!!!] Skipping benchmark recompilation!"
        let listConfigs threadsettings = 
                      [ BenchRun { threads=t, sched=s, bench=b, env=e } | 
			b@(Benchmark {compatScheds}) <- benchlist, 
			s <- S.toList (S.intersection scheds (S.fromList compatScheds)),
			t <- threadsettings,
                        e <- envs]

            allruns = listConfigs threadsettings 
            total = length allruns

            -- All that matters for compilation is nonthreaded (0) or threaded [1,inf)
            pruned = S.toList $ S.fromList $
                     -- Also ARGS and ENV can be ignored for compilation purposes:
                     map (\ (BenchRun { threads, sched, bench })
                              -> BenchRun { threads
                                          , sched
                                          , bench=bench{ args=[] }
                                          , env=[]} ) $ 
                     listConfigs $
                     S.toList $ S.fromList $
                     map (\ x -> if x==0 then 0 else 1) threadsettings

        log$ "\n--------------------------------------------------------------------------------"
        log$ "Running all benchmarks for all thread settings in "++show threadsettings
        log$ "Running for all schedulers in: "++show (S.toList scheds)
        log$ "Testing "++show total++" total configurations of "++ show (length benchlist)++" benchmarks"
        log$ "--------------------------------------------------------------------------------"

        if ParBench `elem` options then do 
        --------------------------------------------------------------------------------
        -- Parallel version:
            lift$ putStrLn$ "[!!!] Compiling in Parallel..."
               
            when recomp $ do 
              
              when hasCabalFile (error "Currently, cabalized build does not support parallelism!")
                          
              -- Version 1: This forks ALL compiles in parallel [REMOVED, CHECK VCS]
              -- Version 2: This uses P worker threads.
              (outputs,killem) <- parForMTwoPhaseAsync (zip [1..] pruned) $ \ (confnum,bench) -> do
                   -- Inside each action, we force the complete evaluation:
                   out@(BL _ lss) <- forkWithBufferedLogs$ compileOne bench (confnum,length pruned)
                   return (lss, forceBuffered out)
                   -- ALTERNATIVE: Simply output directly to stdout/stderr.  MESSY:
    --               compileOne bench (confnum,length pruned)
    --  	         return ((), return ())

              flushBuffered outputs 
              -- We MUST ensure that all other threads are shut down
              -- so that the benchmark.run process doesn't pollute
              -- the benchmark run itself.
              lift$ putStrLn$ "[!!!] BARRIER - waiting for jobs to complete / kill threads before starting real work..."
              liftIO$ killem 

	    if shortrun then do
               lift$ putStrLn$ "[!!!] Running in Parallel..."
	       (outputs,_) <- parForMTwoPhaseAsync (zip [1..] pruned) $ \ (confnum,bench) -> do
		  out@(BL _ ls3) <- forkWithBufferedLogs$ runOne bench (confnum,total)
		  return (ls3, forceBuffered out)
	       flushBuffered outputs 
               return ()
	     else do 
               -- Non-shortrun's NEVER run multiple benchmarks at once:
	       forM_ (zip [1..] allruns) $ \ (confnum,bench) -> 
		    runOne bench (confnum,total)
               return ()

        else do
        --------------------------------------------------------------------------------
        -- Serial version:
          when recomp 
            (if hasCabalFile then do
                log$ "Found cabal file in top-level benchmark dir.  Using it to build all benchmarks: "++cabalFile
                void invokeCabal
             else do              
                log$ "Not using Cabal.  Attempting to build the old fashioned way."             
                forM_ (zip [1..] pruned) $ \ (confnum,bench) -> 
                   compileOne bench (confnum,length pruned))

          forM_ (zip [1..] allruns) $ \ (confnum,bench) -> 
              runOne bench (confnum,total)

        log$ "\n--------------------------------------------------------------------------------"
        log "  Finished with all test configurations."
        log$ "--------------------------------------------------------------------------------"
	liftIO$ exitSuccess
    )
    conf



----------------------------------------------------------------------------------------------------
-- *                                 GENERIC HELPER ROUTINES                                      
----------------------------------------------------------------------------------------------------

-- | These should properly go in another module, but they live here to
--   keep benchmark.hs self contained.


-- | Fork a thread but ALSO set up an error handler.
forkIOH :: String -> Maybe (Buffer String, t, t1) -> IO () -> IO ThreadId
forkIOH who maybhndls action = 
  forkIO $ handle (\ (e::SomeException) -> 
                   case fromException e of
                     Just ThreadKilled -> return ()
                     Nothing -> do 
		        hPutStrLn stderr$ "ERROR: "++who++": Got exception inside forked thread: "++show e
			tid <- readIORef main_threadid
			case maybhndls of 
			  Nothing -> return ()
			  Just (logBuf,resBuf,stdoutBuf) -> 
			     do 
				hPutStrLn stderr "=========================="
				hPutStrLn stderr "Buffered contents for log:"
				hPutStrLn stderr "=========================="
				str <- peekBuffer logBuf
				hPutStrLn stderr (unlines str)
				return ()
			throwTo tid e
		  )
           action


-- | Parallel for loops.
-- 
-- The idea is to keep perform a sliding window of work and to execute
-- on numCapabilities threads, avoiding oversubscription.
-- 
-- This version takes:
--   (1) a two-phase action that returns both a result, and a
--       completion-barrier action.
-- 
-- The result is:
--   (1) a lazy list with implicit blocking and IO.
--   (2) a final barrier/kill action that 
type TwoPhaseAction a b = a -> ReaderT Config IO (b, IO ())
parForMTwoPhaseAsync :: [a] -> TwoPhaseAction a b -> ReaderT Config IO ([b], IO ())
parForMTwoPhaseAsync ls action = 
  do state@Config{maxthreads,outHandles} <- ask
     lift$ do 
       answers <- sequence$ replicate (length ls) newEmptyMVar
       workIn  <- newIORef (zip ls answers)
       tids <- forM [1..numCapabilities] $ \ id -> 
--       forM_ [1..maxthreads] $ \ id -> 
           forkIOH "parFor worker" outHandles $ do 
             -- Pop work off the queue:
             let pfloop = do -- putStrLn$ "Worker "++show id++" looping ..."
                           x <- atomicModifyIORef workIn 
						  (\ls -> if null ls 
							  then ([], Nothing) 
							  else (tail ls, Just (head ls)))
                           case x of 
			     Nothing -> return () -- putMVar finit ()
			     Just (input,mv) -> 
                               do (result,barrier) <- runReaderT (action input) state
				  putMVar mv result
				  barrier 
                                  pfloop
             pfloop     

       -- Read out the answers in order:
       chan <- newChan
       lastTid <- forkIOH "parFor reader thread" outHandles $ 
			  forM_ answers $ \ mv -> do
			    x <- readMVar mv
			    writeChan chan x 
       strm <- getChanContents chan
       let lazyListIO = take (length ls) strm
           -- This can either wait and then kill, or just kill.
           -- Trying a waiting version:
           killem = case length lazyListIO of 
		      _ -> do mapM_ killThread tids
			      killThread lastTid
       return (lazyListIO, killem)


--------------------------------------------------------------------------------
-- * "Buffer"s, A Limited Chan Replacement
--------------------------------------------------------------------------------

-- | Chan's don't quite do the trick.  Here's something simpler.  It
--   keeps a buffer of elemnts and an MVar to signal "end of stream".
--   This it separates blocking behavior from data access.
data Buffer a = Buf (MVar ()) (IORef [a])

newBuffer :: IO (Buffer a)
newBuffer = do
  mv  <- newEmptyMVar
  ref <- newIORef []
  return (Buf mv ref)

writeBuffer :: Buffer a -> a -> IO ()
writeBuffer (Buf mv ref) x = do
  b <- isEmptyMVar mv
  if b
     then atomicModifyIORef ref (\ ls -> (x:ls,()))
   else error "writeBuffer: cannot write to closed Buffer"

-- | Signal completion. 
closeBuffer :: Buffer a -> IO ()
closeBuffer (Buf mv _) = putMVar mv ()

peekBuffer :: Buffer a -> IO [a]
peekBuffer (Buf _ ref) = liftM reverse $ readIORef ref 

-- | Returns a lazy list, just like getChanContents:
getBufferContents :: Buffer a -> IO [a]
getBufferContents buf@(Buf mv ref) = do
  -- A Nothing on this chan means "end-of-stream":
  chan <- newChan 
  let gbcloop = do 
	 grabbed <- atomicModifyIORef ref (\ ls -> ([], reverse ls))
	 mapM_ (writeChan chan . Just) grabbed
	 mayb <- tryTakeMVar mv -- Check if we're done.
	 case mayb of 
	   Nothing -> threadDelay 10000 >> gbcloop
	   Just () -> writeChan chan Nothing
  forkIO gbcloop
  ls <- getChanContents chan
  return (map fromJust $ 
	  takeWhile isJust ls)

----------------------------------------------------------------------------------------------------
-- Treatment of buffered logging using Buffer:
----------------------------------------------------------------------------------------------------

type TriString = (String, String, String)
data BufferedLogs = BL ThreadId TriString

-- | Capture logging output in memory.  Don't write directly to log files.
withBufferedLogs :: ReaderT Config IO a-> ReaderT Config IO TriString
withBufferedLogs action = do
  (hnd,io) <- makeBufferedAction action
  lift io -- Run it immediately.
  return hnd
 where 
  makeBufferedAction :: ReaderT Config IO a-> ReaderT Config IO (TriString, IO())
  makeBufferedAction action = do
      (chans,io) <- helper action
      lss <- convertBufs chans
      return$ (lss, io) 

-- | Forking, asynchronous version.
forkWithBufferedLogs :: ReaderT Config IO a-> ReaderT Config IO BufferedLogs
forkWithBufferedLogs action = do 
--  Config{outHandles} <- ask
  (chans,io) <- helper action
  tid <- lift$ forkIOH "log buffering" (Just chans) io -- Run it asynchronously.
  lss <- convertBufs chans 
  return (BL tid lss)

-- Helper function shared by the above.
-- 
-- Returns lazy output as well as a wrapped up action tha will perform the work item passed in.
-- This action should close the buffers when it is done.
helper action = 
 do conf <- ask 
    [buf1,buf2,buf3] <- lift$ sequence [newBuffer,newBuffer,newBuffer]
    -- Run in a separate thread:
    let io = do runReaderT action 
			   -- (return ())
			   conf{outHandles = Just (buf1,buf2,buf3), 
				logFile    = error "shouldn't use logFile presently", 
				resultsFile= error "shouldn't use resultsFile presently"}
		closeBuffer buf1 
		closeBuffer buf2 
		closeBuffer buf3 
    return$ ((buf1,buf2,buf3), io) 

-- Shared by the routines above.  Convert between buffers and lists with implicit lazyIO.
convertBufs (buf1,buf2,buf3) = do 
  ls1 <- lift$ getBufferContents buf1
  ls2 <- lift$ getBufferContents buf2
  ls3 <- lift$ getBufferContents buf3
  return$ (unlines ls1, unlines ls2, unlines ls3)

-- Make sure that we complete the lazy IO by reading all the way to the end:
forceBuffered :: BufferedLogs -> IO ()
forceBuffered (BL tid (logs,results,stdouts)) = do
  evaluate (length logs)
  evaluate (length results)
  evaluate (length stdouts)
  killThread tid 
  return ()

flushBuffered :: [TriString] -> ReaderT Config IO ()
flushBuffered outputs = do
        Config{logFile,resultsFile} <- ask

        -- Here we want to print output as though the processes were
        -- run in serial.  Interleaved output is very ugly.
        let logOuts    = map fst3 outputs
	    resultOuts = map snd3 outputs
	    stdOuts    = map thd3 outputs

        -- This is the join.  Send all output where it is meant to go:
        lift$ mapM_ putStrLn stdOuts
        lift$ appendFile logFile     (concat logOuts)
        lift$ appendFile resultsFile (concat resultOuts)
        -- * All forked tasks will be finished by this point.
        return ()

----------------------------------------------------------------------------------------------------
