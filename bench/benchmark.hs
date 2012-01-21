#!/usr/bin/env runhaskell
{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, RecordWildCards #-}

-- NOTE: Under 7.2 I'm running into this HSH problem:
-- 
-- benchmark.hs: HSH/Command.hs:(289,14)-(295,45): Missing field in record construction System.Process.Internals.create_group

{- 
   Runs benchmarks.  (Note Simon Marlow has his another setup, but this
   one is self contained.)

   ---------------------------------------------------------------------------
   Usage: [set env vars] ./benchmark.hs

   Call it with the following environment variables...

     SHORTRUN=1 to get a shorter run for testing rather than benchmarking.

     THREADS="1 2 4" to run with # threads = 1, 2, or 4.

     KEEPGOING=1 to keep going after the first error.

     TRIALS=N to control the number of times each benchmark is run.

     BENCHLIST=foo.txt to select the benchmarks and their arguments
		       (uses benchlist.txt by default)

     SCHEDS="Trace Direct Sparks ContFree" -- Restricts to a subset of schedulers.

     GENERIC=1 to go through the generic (type class) monad par
               interface instead of using each scheduler directly

   Additionally, this script will propagate any flags placed in the
   environment variables $GHC_FLAGS and $GHC_RTS.  It will also use
   $GHC, if available, to select the $GHC executable.

   ---------------------------------------------------------------------------

      ----
   << TODO >>
      ====

   * Factor out compilation from execution so that compilation can be parallelized.
     * Further enable packing up a benchmark set to run on a machine
       without GHC (as with Haskell Cnc)
   * Replace environment variable argument passing with proper flags/getopt.

   * Handle testing with multiple GHC versions and multiple flag-configs.

-}

import System.Environment
import System.Directory
import System.Random -- (randomIO)
import System.Exit
import System.FilePath (splitFileName, (</>))
import System.Process (system)
--import GHC.Conc (numCapabilities)
import HSH
import Prelude hiding (log)
import Control.Monad.Reader
import Text.Printf
import Debug.Trace
import Data.Char (isSpace)
import Data.Word (Word64)
import qualified Data.Set as S
import Data.List (isPrefixOf, tails, isInfixOf, delete)

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
 , ghc_flags      :: String
 , ghc_RTS        :: String -- +RTS flags
 , scheds         :: S.Set Sched -- subset of schedulers to test.
 , hostname       :: String 
 , resultsFile    :: String -- Where to put timing results.
 , logFile        :: String -- Where to put more verbose testing output.
 }


-- Represents a configuration of an individual run.
--  (number of
-- threads, other flags, etc):
data BenchRun = BenchRun
 { threads :: Int
 , sched   :: Sched 
 , bench   :: Benchmark
 } deriving (Eq, Show)

data Sched 
   = Trace | Direct | Sparks | ContFree
   | None
 deriving (Eq, Show, Read, Ord)

allScheds = S.fromList [Trace, Direct, Sparks, ContFree, None]

data Benchmark = Benchmark
 { name :: String
 , compatScheds :: [Sched]
 , args :: [String]
 } deriving (Eq, Show)

-- Name of a script to time N runs of a program:
-- (I used a haskell script for this but ran into problems at one point):
-- ntimes = "./ntimes_binsearch.sh"
ntimes = "./ntimes_minmedmax"


--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- Retrieve the configuration from the environment.
getConfig = do
  hostname <- runSL$ "hostname -s"
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
		"" -> allScheds
		s  -> S.fromList (map read (words s))

  case get "GENERIC" "" of 
    "" -> return ()
    s  -> error$ "GENERIC env variable not handled yet.  Set to: " ++ show s

  ----------------------------------------
  -- Determine the number of cores.
  d <- doesDirectoryExist "/sys/devices/system/cpu/"
  uname <- runSL "uname"
  maxthreads :: String 
       <- if d 
	  then runSL$ "ls  /sys/devices/system/cpu/" -|- egrep "cpu[0123456789]*$" -|- wcL
	  else if uname == "Darwin"
	  then runSL$ "sysctl -n hw.ncpu"
	  else error$ "Don't know how to determine the number of threads on platform: "++ show uname
                -- TODO: Windows!
  -- Note -- how do we find the # of threads ignoring hyperthreading?
  ----------------------------------------

  logOn logFile$ "Reading list of benchmarks/parameters from: "++bench
  benchstr <- readFile bench
  let ver = case filter (isInfixOf "ersion") (lines benchstr) of 
	      (h:t) -> read $ head $ filter isNumber (words h)
	      []    -> 0

  -- Here are the DEFAULT VALUES:
  return$ 
    Config { hostname, logFile, scheds, shortrun    
	   , ghc        =       get "GHC"       "ghc"
	   , ghc_RTS    =       get "GHC_RTS" (if shortrun then "" else "-qa -s")
  	   , ghc_flags  = (get "GHC_FLAGS" (if shortrun then "" else "-O2")) 
	                  ++ " -rtsopts" -- Always turn on rts opts.
	   , trials         = read$ get "TRIALS"    "1"
	   , benchlist      = parseBenchList benchstr
	   , benchversion   = (bench, ver)
	   , maxthreads     = read maxthreads
	   , threadsettings = parseIntList$ get "THREADS" maxthreads	   
	   , keepgoing      = strBool (get "KEEPGOING" "0")
	   , resultsFile    = "results_" ++ hostname ++ ".dat"
	   }

pruneThreadedOpts :: [String] -> [String]
pruneThreadedOpts = filter (`notElem` ["-qa", "-qb"])

-- Does a change in settings require recompilation?
recompRequired (BenchRun t1 s1 b1) (BenchRun t2 s2 b2) =
 if b1 /= b2 || s1 /= s2 then True else
 case (t1,t2) of
   -- Switching from serial to parallel or back requires recompile:
   (0,t) | t > 0  -> True 
   (t,0) | t > 0  -> True 
   -- Otherwise, no recompile:
   (last,current) -> False

benchChanged (BenchRun _ _ b1) (BenchRun _ _ b2) = b1 /= b2

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

expandMode s = error$ "Unknown Scheduler or mode: " ++s

-- Omitting Direct until its bugs are fixed:
ivarScheds = [Trace, ContFree, Direct] 

schedToModule s = 
  case s of 
--   Trace    -> "Control.Monad.Par"
   Trace    -> "Control.Monad.Par.Scheds.Trace"
   Direct   -> "Control.Monad.Par.Scheds.Direct"
   ContFree -> "Control.Monad.Par.Scheds.ContFree"
   Sparks   -> "Control.Monad.Par.Scheds.Sparks"
   None     -> "qualified Control.Monad.Par as NotUsed"
  

--------------------------------------------------------------------------------
-- Misc Small Helpers

-- These int list arguments are provided in a space-separated form:
parseIntList :: String -> [Int]
parseIntList = map read . words 

-- Remove whitespace from both ends of a string:
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- Create a sliding window over the list.  The last element of each
-- window is the "current position" and samples each element of the
-- original list once.  Any additional elements are "history".
slidingWin w ls = 
  reverse $ map reverse $ 
  filter (not . null) $ 
  map (take w) (tails$ reverse ls)

prop1 w ls = map (head . reverse) (slidingWin w ls) == ls
t1 = prop1 3  [1..50]
t2 = prop1 30 [1..20]

parseBenchList str = 
  map parseBench $                 -- separate operator, operands
  filter (not . null) $            -- discard empty lines
  map words $ 
  filter (not . isPrefixOf "#") $  -- filter comments
  map trim $
  lines str

parseBench (h:m:tl) = Benchmark {name=h, compatScheds=expandMode m, args=tl }
parseBench ls = error$ "entry in benchlist does not have enough fields (name mode args): "++ unwords ls

strBool ""  = False
strBool "0" = False
strBool "1" = True
strBool  x  = error$ "Invalid boolean setting for environment variable: "++x

inDirectory dir action = do
  d1 <- liftIO$ getCurrentDirectory
  liftIO$ setCurrentDirectory dir
  x <- action
  liftIO$ setCurrentDirectory d1
  return x
  
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

runIgnoreErr :: String -> IO String
runIgnoreErr cm = 
  do (str,force) <- run cm
     (err::String, code::ExitCode) <- force
     return str

-- Based on a benchmark configuration, come up with a unique suffix to
-- distinguish the executable.
uniqueSuffix BenchRun{threads,sched,bench} =    
  "_" ++ show sched ++ 
   if threads == 0 then "_serial"
                   else "_threaded"
-- where 
--  Bench {name,args} = bench


--------------------------------------------------------------------------------
-- Error handling
--------------------------------------------------------------------------------

-- Check the return code from a call to a test executable:
check ExitSuccess _           = return True
check (ExitFailure code) msg  = do
  Config{..} <- ask
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
          lift$ exit code
  return False

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------


-- Stdout and logFile:
log :: String -> ReaderT Config IO ()
log str = do
  Config{logFile} <- ask
  lift$ logOn logFile str

-- Stdout, logFile, and resultsFile
logBoth str = do log str; logR str 

-- Results file only:
logR str = do 
  Config{resultsFile} <- ask
  lift$ runIO$ echo (str++"\n") -|- appendTo resultsFile

logOn :: String -> String -> IO ()
logOn file s = 
  runIO$ echo (s++"\n") -|- tee ["/dev/stdout"] -|- appendTo file

backupResults Config{resultsFile, logFile} = do 
  e    <- doesFileExist resultsFile
  --  date <- runSL "date +%s"
--  date <- runSL "date +%Y%m%d_%R"
  date <- runSL "date +%Y%m%d_%s"
  when e $ do
    renameFile resultsFile (resultsFile ++"."++date++".bak")
  e2   <- doesFileExist logFile
  when e2 $ do
    renameFile logFile     (logFile     ++"."++date++".bak")

--------------------------------------------------------------------------------
-- Compiling Benchmarks
--------------------------------------------------------------------------------

compileOne :: BenchRun -> (Int,Int) -> ReaderT Config IO Bool
compileOne br@(BenchRun numthreads sched (Benchmark test _ args_)) 
	      (iterNum,totalIters) = 
  do Config{ghc, ghc_flags, shortrun} <- ask

     let flags_ = case numthreads of
		   0 -> ghc_flags
		   _ -> ghc_flags++" -threaded"
	 flags = flags_ ++ " -fforce-recomp -DPARSCHED=\""++ (schedToModule sched) ++ "\""

	 (containingdir,_) = splitFileName test
	 hsfile = test++".hs"
	 exefile = test++ uniqueSuffix br ++ ".exe"
         args = if shortrun then shortArgs args_ else args_

     log$ "\n--------------------------------------------------------------------------------"
     log$ "  Compiling Config "++show iterNum++" of "++show totalIters++
	  ": "++test++" (args \""++unwords args++"\") scheduler "++show sched ++ 
           if numthreads==0 then " serial" else " threaded"
     log$ "--------------------------------------------------------------------------------\n"

     e  <- lift$ doesFileExist hsfile
     d  <- lift$ doesDirectoryExist containingdir
     mf <- lift$ doesFileExist$     containingdir </> "Makefile"
     if e then do 
	 log "Compiling with a single GHC command: "
	 let cmd = unwords [ghc, "--make", "-i../", "-i"++containingdir, flags, 
			    hsfile, "-o "++exefile]		
	 log$ "  "++cmd ++"\n"
	 -- Having trouble getting the &> redirection working.  Need to specify bash specifically:
         tmpfile <- mktmpfile
	 code <- lift$ system$ "bash -c "++show (cmd++" &> "++tmpfile)
	 flushtmp tmpfile 
	 check code "ERROR, benchmark.hs: compilation failed."


     else if (d && mf && containingdir /= ".") then do 
	log " ** Benchmark appears in a subdirectory with Makefile.  Using it."
	log " ** WARNING: Can't be sure to control compiler options for this benchmark!"
	log " **          (Hopefully it will obey the GHC_FLAGS env var.)"
	log$ " **          (Setting GHC_FLAGS="++ flags++")"
	inDirectory containingdir $ do
           -- First we make clean because we can't trust the makefile to rebuild when flags change:
	   code1 <- lift$ run "make clean" 
	   check code1 "ERROR, benchmark.hs: Benchmark's 'make clean' failed"
	   code2 <- lift$ run$ setenv [("GHC_FLAGS",flags)] "make"
	   check code2 "ERROR, benchmark.hs: Compilation via benchmark Makefile failed:"

     else do 
	log$ "ERROR, benchmark.hs: File does not exist: "++hsfile
	lift$ exit 1

--------------------------------------------------------------------------------
-- Running Benchmarks
--------------------------------------------------------------------------------

-- If the benchmark has already been compiled doCompile=False can be
-- used to skip straight to the execution.
runOne :: BenchRun -> (Int,Int) -> ReaderT Config IO ()
runOne br@(BenchRun numthreads sched (Benchmark test _ args_)) 
          (iterNum,totalIters) = do
  Config{..} <- ask
  let args = if shortrun then shortArgs args_ else args_
  
  log$ "\n--------------------------------------------------------------------------------"
  log$ "  Running Config "++show iterNum++" of "++show totalIters++
       ": "++test++" (args \""++unwords args++"\") scheduler "++show sched++"  threads "++show numthreads
  log$ "--------------------------------------------------------------------------------\n"
  pwd <- lift$ getCurrentDirectory
  log$ "(In directory "++ pwd ++")"

  log$ "Next run who, reporting users other than the current user.  This may help with detectivework."
--  whos <- lift$ run "who | awk '{ print $1 }' | grep -v $USER"
  whos <- lift$ run$ "who" -|- map (head . words)
  user <- lift$ getEnv "USER"

  log$ "Who_Output: "++ unwords (filter (/= user) whos)

  -- numthreads == 0 indicates a serial run:
  let 
      rts = case numthreads of
	     0 -> ghc_RTS
	     _ -> ghc_RTS  ++" -N"++show numthreads
      exefile = "./" ++ test ++ uniqueSuffix br ++ ".exe"
  ----------------------------------------
  -- Now Execute:
  ----------------------------------------

  -- If we failed compilation we don't bother running either:
  let prunedRTS = unwords (pruneThreadedOpts (words rts)) -- ++ "-N" ++ show numthreads
      ntimescmd = printf "%s %d %s %s +RTS %s -RTS" ntimes trials exefile (unwords args) prunedRTS
  log$ "Executing " ++ ntimescmd

  -- One option woud be dynamic feedback where if the first one
  -- takes a long time we don't bother doing more trials.  

  tmpfile <- mktmpfile
  -- NOTE: With this form we don't get the error code.  Rather there will be an exception on error:
  (str::String,finish) <- lift$ run (ntimescmd ++" 2> "++ tmpfile) -- HSH can't capture stderr presently
  (_  ::String,code)   <- lift$ finish                       -- Wait for child command to complete
  flushtmp tmpfile
  check code ("ERROR, benchmark.hs: test command \""++ntimescmd++"\" failed with code "++ show code)

  let times = 
       case code of
	ExitSuccess     -> str
	ExitFailure 143 -> "TIMEOUT TIMEOUT TIMEOUT"
	-- TEMP: [2012.01.16], ntimes is for some reason getting 15 instead of 143.  HACKING this temporarily:
	ExitFailure 15  -> "TIMEOUT TIMEOUT TIMEOUT"
	ExitFailure _   -> "ERR ERR ERR"		     

  log $ " >>> MIN/MEDIAN/MAX TIMES " ++ times
  logR$ test ++" "++ show sched ++" "++ show numthreads ++" "++ trim times

  return ()
  

-- Helpers for creating temporary files:
------------------------------------------------------------
mktmpfile = do 
   n :: Word64 <- lift$ randomIO
   return$ "._Temp_output_buffer_"++show (n)++".txt"
-- Flush the temporary file to the log file (deleting it in the process):
flushtmp tmpfile = 
           do Config{shortrun, logFile} <- ask
              lift$ runIO$ catFrom [tmpfile] -|- indent -|- appendTo logFile
	      unless shortrun $ 
		 lift$ runIO$ catFrom [tmpfile] -|- indent -- To stdout
	      lift$ removeFile tmpfile
-- Indent for prettier output
indent = map ("    "++)
------------------------------------------------------------


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
  branch   <- runIgnoreErr "git name-rev --name-only HEAD"
  revision <- runIgnoreErr "git rev-parse HEAD"
  -- Note that this will NOT be newline-terminated:
  hashes   <- runIgnoreErr "git log --pretty=format:'%H'"
  mapM_ runIO $ 
   [
     e$ "# TestName Variant NumThreads   MinTime MedianTime MaxTime"        
   , e$ "#    "        
   , e$ "# `date`"
   , e$ "# `uname -a`" 
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
   , e$ "#  ENV THREADS=$THREADS"
   , e$ "#  ENV TRIALS=$TRIALS"
   , e$ "#  ENV SHORTRUN=$SHORTRUN"
   , e$ "#  ENV SCHEDS=$SCHEDS"
   , e$ "#  ENV KEEPGOING=$KEEPGOING"
   , e$ "#  ENV GHC=$GHC"
   , e$ "#  ENV GHC_FLAGS=$GHC_FLAGS"
   , e$ "#  ENV GHC_RTS=$GHC_RTS"
   ]
 where 
    e s = ("echo \""++s++"\"") -|- tee ["/dev/stdout", logFile] -|- appendTo resultsFile


----------------------------------------------------------------------------------------------------
-- Main Script
----------------------------------------------------------------------------------------------------


data ActionList = 
   Run Bool BenchRun -- Bool signifies recompile required.
 | Print String


main = do

  -- HACK: with all the inter-machine syncing and different version
  -- control systems I run into permissions problems sometimes:
  system "chmod +x ./ntime* ./*.sh"

  conf@Config{..} <- getConfig    

  runReaderT 
    (do         

        lift$ backupResults conf
	log "Writing header for result data file:"
	lift$ resultsHeader conf 

	log "Before testing, first 'make clean' for hygiene."
	-- Hah, make.out seems to be a special name of some sort, this actually fails:
	--	code <- lift$ system$ "make clean &> make.out"
	code <- lift$ system$ "make clean &> make_output.tmp"
	check code "ERROR: 'make clean' failed."
	log " -> Succeeded."
	liftIO$ removeFile "make_output.tmp"

        let genConfigs threadsettings = 
                      [ BenchRun t s b | 
			b@(Benchmark {compatScheds}) <- benchlist, 
			s <- S.toList (S.intersection scheds (S.fromList compatScheds)),
			t <- threadsettings ]

            allruns = genConfigs threadsettings
            total = length allruns

            -- All that matters for compilation is nonthreaded (0) or threaded [1,inf)
            pruned = genConfigs $
                     S.toList $ S.fromList $
                     map (\ x -> if x==0 then 0 else 1) threadsettings

        log$ "\n--------------------------------------------------------------------------------"
        log$ "Running all benchmarks for all thread settings in "++show threadsettings
        log$ "Testing "++show total++" total configurations of "++ show (length benchlist) ++" benchmarks"
        log$ "--------------------------------------------------------------------------------"

        forM_ (zip [1..] pruned) $ \ (confnum,bench) -> do
--	      let (BenchRun _ _ (Benchmark test _ args)) = bench
              compileOne bench (confnum,length pruned)

        forM_ (zip [1..] allruns) $ \ (confnum,bench) -> do
	      let (BenchRun _ _ (Benchmark test _ args)) = bench
	      runOne bench (confnum,total)
{-
        forM_ (zip [1..] (slidingWin 2 allruns)) $ \ (confnum,win) -> do
              let (recomp,newbench) = 
                        case win of 
			     [x]   -> (True,True) -- First run, must compile.
			     [a,b] -> (recompRequired a b, benchChanged a b)
		  bench@(BenchRun _ _ (Benchmark test _ args)) = head$ reverse win
              when recomp   $ log "Recompile required for next config:"
              when newbench $ logR$ "\n# *** Config ["++show confnum++"..?], testing with ./"++ test ++".exe "++unwords args

	      success <- if recomp then compileOne bench (confnum,total)
	                           else return True
              if success 
               then runOne bench (confnum,total)
	       else log$ "Compile failed, not running benchmark for config "++show confnum
-}

        log$ "\n--------------------------------------------------------------------------------"
        log "  Finished with all test configurations."
        log$ "--------------------------------------------------------------------------------"
	liftIO$ exitSuccess
    )
    conf


