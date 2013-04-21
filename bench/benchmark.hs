{-# LANGUAGE BangPatterns, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleContexts #-}
{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
-- NOTE: This is best when compiled with "ghc -threaded"
-- However, ideally for real benchmarking runs we WANT the waitForProcess below block the whole process.
-- However^2, currently [2012.05.03] when running without threads I get errors like this:
--   benchmark.run: bench_hive.log: openFile: resource busy (file is locked)

--------------------------------------------------------------------------------


{- |
   
benchmark.hs
------------

This program runs a set of benchmarks contained in the current
directory.  It produces two files as output:

    results_HOSTNAME.dat
    bench_HOSTNAME.log


            ASSUMPTIONS -- about directory and file organization
            ----------------------------------------------------

This benchmark harness can run either cabalized benchmarks, or
straight .hs files buildable by "ghc --make".


   
---------------------------------------------------------------------------
                                << TODO >>
   ---------------------------------------------------------------------------

   * Replace environment variable argument passing with proper flags/getopt.

   <Things that worked at one time but need to be cleaned up:>
     
     * Further enable packing up a benchmark set to run on a machine
       without GHC (as with Haskell Cnc)
     
     * Clusterbench -- adding an additional layer of parameter variation.

-}

module Main where 

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
import qualified Data.Set as Set
import Data.List (isPrefixOf, tails, isInfixOf, delete)
import System.Console.GetOpt (getOpt, ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.Environment (getArgs, getEnv, getEnvironment, getExecutablePath)
import System.Directory
import System.Posix.Env (setEnv)
import System.Random (randomIO)
import System.Exit
import System.FilePath (splitFileName, (</>), takeDirectory)
import System.Process (system, waitForProcess, getProcessExitCode, runInteractiveCommand, 
                       createProcess, CreateProcess(..), CmdSpec(..), StdStream(..), readProcess)
import System.IO (Handle, hPutStrLn, stderr, openFile, hClose, hGetContents, hIsEOF, hGetLine,
                  IOMode(..), BufferMode(..), hSetBuffering)
import System.IO.Unsafe (unsafePerformIO)

import qualified System.IO.Streams as Strm
import qualified System.IO.Streams.Concurrent as Strm
import qualified Data.ByteString.Char8 as B

import UI.HydraPrint (hydraPrint, HydraConf(..), DeleteWinWhen(..), defaultHydraConf, hydraPrintStatic)
import Scripting.Parallel.ThreadPool (parForM)

import Text.Printf

#define FUSION_TABLES
#ifdef FUSION_TABLES
import Network.Google.OAuth2 (getCachedTokens, OAuth2Client(..), OAuth2Tokens(..))
import Network.Google.FusionTables (createTable, listTables, listColumns, insertRows,
                                    TableId, CellType(..), TableMetadata(..))
#endif

----------------------------------------------------------------------------------------------------


-- | USAGE
usageStr = unlines $
 [
--   "USAGE: [set environment vars] ./benchmark.hs [--no-recomp, --par]",
--   "\nUSAGE: [set environment vars] ./benchmark.hs [CMDLN OPTIONS]",
   "\n ENV VARS:",
   "   These environment variables control the behavior of the benchmark script:",
   "",
   "     SHORTRUN=1 to get a shorter run for testing rather than benchmarking.",
   "",
   "     THREADS=\"1 2 4\" to run with # threads = 1, 2, or 4.",
   "",
   "     KEEPGOING=1 to keep going after the first error.",
   "",
   "     TRIALS=N to control the number of times each benchmark is run.",
   "",
   "     BENCHLIST=foo.txt to select the benchmarks and their arguments",
   "               (uses benchlist.txt by default)",
   "",
   "     SCHEDS=\"Trace Direct Sparks\" -- Restricts to a subset of schedulers.",
   "",
   "     GENERIC=1 to go through the generic (type class) monad par",
   "               interface instead of using each scheduler directly",
   " ",
   "     ENVS='[[(\"KEY1\", \"VALUE1\")], [(\"KEY1\", \"VALUE2\")]]' to set",
   "     different configurations of environment variables to be set *at",
   "     runtime*. Useful for NUMA_TOPOLOGY, for example.  Note that this",
   "     can change multiple env variables in multiple distinct",
   "     configurations, with each configuration tested separately.",
   "",
   "   Additionally, this script will propagate any flags placed in the",
   "   environment variables $GHC_FLAGS and $GHC_RTS.  It will also use",
   "   $GHC, if available, to select the $GHC executable."
 ]

----------------------------------------------------------------------------------------------------

-- The global configuration for benchmarking:
data Config = Config 
 { benchlist      :: [Benchmark]
 , benchsetName   :: Maybe String -- ^ What identifies this set of benchmarks?  Used to create fusion table.
 , benchversion   :: (String, Double) -- ^ benchlist file name and version number (e.g. X.Y)
 , threadsettings :: [Int]  -- ^ A list of #threads to test.  0 signifies non-threaded mode.
 , maxthreads     :: Int
 , trials         :: Int    -- ^ number of runs of each configuration
 , shortrun       :: Bool
 , keepgoing      :: Bool   -- ^ keep going after error
 , ghc            :: String -- ^ ghc compiler path
 , ghc_pkg        :: String
 , ghc_flags      :: String
 , ghc_RTS        :: String -- ^ +RTS flags
 , scheds         :: Set.Set Sched -- ^ subset of schedulers to test.
 , hostname       :: String 
 , resultsFile    :: String -- ^ Where to put timing results.
 , logFile        :: String -- ^ Where to put more verbose testing output.
 
 -- These are all LINES-streams (implicit newlines).
 , logOut         :: Strm.OutputStream B.ByteString
 , resultsOut     :: Strm.OutputStream B.ByteString
 , stdOut         :: Strm.OutputStream B.ByteString
   -- A set of environment variable configurations to test
 , envs           :: [[(String, String)]]
   
#ifdef FUSION_TABLES
 , doFusionUpload :: Bool
 , fusionTableID  :: Maybe TableId -- ^ This must be Just whenever doFusionUpload is true.
 , fusionClientID :: Maybe String
 , fusionClientSecret :: Maybe String
--  , fusionUpload   :: Maybe FusionInfo
#endif
 }
 deriving Show

-- -- | If we are uploading to a fusion table, we need ALL three of these pieces:
-- data FusionInfo = FusionInfo { tableID :: TableId, 
--                                cliID :: String,
--                                cliSec :: String }

instance Show (Strm.OutputStream a) where
  show _ = "<OutputStream>"


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
ntimes = takeDirectory (unsafePerformIO getExecutablePath) </> "ntimes_minmedmax"

gc_stats_flag = " -s " 
-- gc_stats_flag = " --machine-readable -t "

exedir = "./bin"

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- Retrieve the (default) configuration from the environment, it may
-- subsequently be tinkered with.  This procedure should be idempotent.
getConfig :: [Flag] -> IO Config
getConfig cmd_line_options = do
  hostname <- runSL$ "hostname -s"
  env      <- getEnvironment

  let get v x = case lookup v env of 
		  Nothing -> x
		  Just  s -> s

      benchF = get "BENCHLIST" "benchlist.txt"
      logFile = "bench_" ++ hostname ++ ".log"
      resultsFile = "results_" ++ hostname ++ ".dat"      
      shortrun = strBool (get "SHORTRUN"  "0")
  -- We can't use numCapabilities as the number of hardware threads
  -- because this script may not be running in threaded mode.

  let scheds = case get "SCHEDS" "" of 
		"" -> defaultSchedSet
		s  -> Set.fromList (map read (words s))

  case get "GENERIC" "" of 
    "" -> return ()
    s  -> error$ "GENERIC env variable not handled yet.  Set to: " ++ show s
  
  maxthreads <- getNumberOfCores
  benchstr   <- readFile benchF

  backupResults resultsFile logFile

  rhnd <- openFile resultsFile WriteMode 
  lhnd <- openFile logFile     WriteMode

  hSetBuffering rhnd NoBuffering
  hSetBuffering lhnd NoBuffering  
  
  resultsOut <- Strm.unlines =<< Strm.handleToOutputStream rhnd
  logOut     <- Strm.unlines =<< Strm.handleToOutputStream lhnd
  stdOut     <- Strm.unlines Strm.stdout
      
  let -- Messy way to extract the benchlist version:
      ver = case filter (isInfixOf "ersion") (lines benchstr) of 
	      (h:t) -> read $ (\ (h:_)->h) $ filter isNumber (words h)
	      []    -> 0
      base_conf = Config 
           { hostname, scheds, shortrun
           , benchsetName = Nothing
	   , ghc        =       get "GHC"       "ghc"
           , ghc_pkg    =       get "GHC_PKG"   "ghc-pkg"
	   , ghc_RTS    =       get "GHC_RTS"   ("-qa " ++ gc_stats_flag) -- Default RTS flags.
  	   , ghc_flags  = (get "GHC_FLAGS" (if shortrun then "" else "-O2")) 
	                  ++ " -rtsopts" -- Always turn on rts opts.
	   , trials         = read$ get "TRIALS"    "1"
	   , benchlist      = parseBenchList benchstr
	   , benchversion   = (benchF, ver)
	   , maxthreads     = maxthreads
	   , threadsettings = parseIntList$ get "THREADS" (show maxthreads)
	   , keepgoing      = strBool (get "KEEPGOING" "0")
	   , resultsFile, logFile, logOut, resultsOut, stdOut         
--	   , outHandles     = Nothing
           , envs           = read $ get "ENVS" "[[]]"
#ifdef FUSION_TABLES
           , doFusionUpload = False
           , fusionTableID  = Nothing 
           , fusionClientID = Nothing
           , fusionClientSecret = Nothing
#endif                              
	   }

  -- Process command line arguments to add extra cofiguration information:
  let doFlag (BenchsetName name) r     = r { benchsetName= Just name }
#ifdef FUSION_TABLES
      doFlag (ClientID cid)   r = r { fusionClientID     = Just cid }
      doFlag (ClientSecret s) r = r { fusionClientSecret = Just s }
      doFlag (FusionTables m) r = 
         let r2 = r { doFusionUpload = True } in
         case m of 
           Just tid -> r2 { fusionTableID = Just tid }
           Nothing -> r2
#endif
      -- Ignored options:
      doFlag ShowHelp r = r
      doFlag NoRecomp r = r
      doFlag NoCabal  r = r
      doFlag NoClean  r = r
      doFlag ParBench r = r
      --------------------
      conf = foldr ($) base_conf (map doFlag cmd_line_options)

#ifdef FUSION_TABLES
  finalconf <- if not (doFusionUpload conf) then return conf else
               case (benchsetName conf, fusionTableID conf) of
                (Nothing,Nothing) -> error "No way to find which fusion table to use!  No name given and no explicit table ID."
                (_, Just tid) -> return conf
                (Just name,_) -> do
                  let auth = OAuth2Client { clientId= fromJust (fusionClientID conf)
                                          , clientSecret= fromJust (fusionClientSecret conf) }
                  tid <- getTableId auth name
                  return conf{fusionTableID= Just tid}
#else
  let finalconf = conf      
#endif         
  runReaderT (log$ "Read list of benchmarks/parameters from: "++benchF) finalconf
  return finalconf

-- TODO: Support Windows!
getNumberOfCores :: IO Int
getNumberOfCores = do 
  -- Determine the number of cores.
  d <- doesDirectoryExist "/sys/devices/system/cpu/"
  uname <- runSL "uname"
  str :: String 
       <- if d 
	  then runSL$ "ls  /sys/devices/system/cpu/ | egrep \"cpu[0123456789]*$\" | wc -l" 
	  else if uname == "Darwin"
	  then runSL$ "sysctl -n hw.ncpu"
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

-- [2012.05.03] RRN: ContFree is not exposed, thus removing it from the
-- default set, though you can still ask for it explicitly:
defaultSchedSet = Set.difference (Set.fromList [minBound ..])
                               (Set.fromList [ContFree, NUMA, SMP])


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
data LogDest = ResultsFile | LogFile | StdOut deriving Show

-- | Print a message (line) both to stdout and logFile:
log :: String -> ReaderT Config IO ()
log = logOn [LogFile,StdOut] -- The commonly used default. 

-- | Log a line to a particular file and also echo to stdout.
logOn :: [LogDest] -> String -> ReaderT Config IO ()
logOn modes s = do
  let bstr = B.pack s -- FIXME
  Config{logOut, resultsOut, stdOut} <- ask
  let go ResultsFile = Strm.write (Just bstr) resultsOut 
      go LogFile     = Strm.write (Just bstr) logOut     
      go StdOut      = Strm.write (Just bstr) stdOut
  liftIO$ mapM_ go modes

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


path :: [FilePath] -> FilePath
path [] = ""
path ls = foldl1 (</>) ls

--------------------------------------------------------------------------------
-- Compiling Benchmarks
--------------------------------------------------------------------------------

-- | Invoke cabal for all of the schedulers in the current config
invokeCabal :: ReaderT Config IO Bool
invokeCabal = do
  Config{ghc, ghc_pkg, ghc_flags, scheds} <- ask  
  bs <- forM (Set.toList scheds) $ \sched -> do
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
  do Config{ghc, ghc_flags, shortrun, logOut, resultsOut, stdOut} <- ask

     uid :: Word64 <- lift$ randomIO
     let flags_ = case numthreads of
		   0 -> ghc_flags
		   _ -> ghc_flags++" -threaded"
	 flags = flags_ ++ " -fforce-recomp -DPARSCHED=\""++ (schedToModule sched) ++ "\""         
	 (diroffset,testRoot) = splitFileName testPath         
         -- TODO: make this command-line configurable.
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
         code <- liftIO $ do 
           (_stdinH, stdoutH, stderrH, pid) <- runInteractiveCommand cmd
           inS    <- Strm.lines =<< Strm.handleToInputStream stdoutH
           errS   <- Strm.lines =<< Strm.handleToInputStream stderrH
           merged <- Strm.concurrentMerge [inS,errS]
  --       (out1,out2) <- Strm.tee merged
           -- Need to TEE to send to both stdout and log....
           -- Send out2 to logFile...
           Strm.supply merged stdOut -- Feed interleaved LINES to stdout.
           waitForProcess pid

	 check False code "ERROR, benchmark.hs: compilation failed."

     -- else if (d && mf && diroffset /= ".") then do
     --    log " ** Benchmark appears in a subdirectory with Makefile.  NOT supporting Makefile-building presently."
     --    error "No makefile-based builds supported..."
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
  conf@Config{..} <- ask
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
  whos <- lift$ runLines$ "who"
  let whos' = map ((\ (h:_)->h) . words) whos
  user <- lift$ getEnv "USER"
  log$ "Who_Output: "++ unwords (filter (/= user) whos')

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
#ifdef FUSION_TABLES
  when doFusionUpload $ liftIO$ do
    let cid    = "679570803037.apps.googleusercontent.com"
        secret = "FnLhDezKFlHL46XLbCf7Ik1L"
        client = OAuth2Client { clientId = cid, clientSecret = secret }
    toks  <- getCachedTokens client
    let [t1,t2,t3] = ts
        [p1,p2,p3] = prods
    insertRows (B.pack$ accessToken toks) (fromJust fusionTableID)
       defaultColumns
       [[testRoot, unwords args, show numthreads, t1,t2,t3, p1,p2,p3]]
    return ()       
#endif

#ifdef FUSION_TABLES
resultsSchema :: [(String, CellType)]
resultsSchema =
  [ ("PROGNAME",STRING)
  , ("VARIANT",STRING)
  , ("ARGS",STRING)
  ]

-- Get the table ID that has been cached on disk, or find the the table in the users
-- Google Drive, or create a new table if needed.
getTableId :: OAuth2Client -> String -> IO TableId
getTableId auth tablename = do
  toks <- getCachedTokens auth
  allTables <- listTables (B.pack$ accessToken toks)

  case filter (\ t -> tab_name t == tablename) allTables of
    [] -> do -- log $ " No table with name "++show tablename ++" found, creating..."
             error "FINISH: create table"
    [t] -> return (tab_tableId t)
    ls  -> error$ " More than one table with name "++show tablename++"!\n "++show ls
  error "FINISH getTableId"
#endif

defaultColumns =
  ["Program","Args","Threads","Sched","Threads",
   "MinTime","MedianTime","MaxTime", "MinTime_Prod","MedianTime_Prod","MaxTime_Prod"]

-- Indent for prettier output
indent :: [String] -> [String]
indent = map ("    "++)
------------------------------------------------------------

-- Helper for launching processes with logging and error checking
-----------------------------------------------------------------
-- [2012.05.03] HSH has been causing no end of problems in the
-- subprocess-management department.  Here we instead use the
-- underlying createProcess library function:
runCmdWithEnv :: Bool -> [(String, String)] -> [Char]
              -> ReaderT Config IO (String, ExitCode)
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
  do lns <- runLines cm
     return (unlines lns)
-----------------------------------------------------------------

-- | Create a thread that echos the contents of a Handle as it becomes
--   available.  Then return all text read through an MVar when the
--   handle runs dry.
echoThread :: Bool -> Handle -> ReaderT Config IO (MVar String)
echoThread echoStdout hndl = do
  mv   <- lift$ newEmptyMVar
  conf <- ask
  lift$ void$ forkIOH "echo thread"  $ 
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

-- | Write the results header out stdout and to disk.
printBenchrunHeader :: ReaderT Config IO ()
printBenchrunHeader = do
  Config{ghc, trials, ghc_flags, ghc_RTS, maxthreads, logOut, resultsOut, stdOut, benchversion, shortrun } <- ask
  liftIO $ do   
    let (benchfile, ver) = benchversion
    -- There has got to be a simpler way!
    -- branch   <- runIgnoreErr "git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'"
    -- branch <- "git symbolic-ref HEAD"
    branch   <- runSL  "git name-rev --name-only HEAD"
    revision <- runSL  "git rev-parse HEAD"
    -- Note that this will NOT be newline-terminated:
    hashes   <- runLines "git log --pretty=format:'%H'"
    let ls :: [IO String]
        ls = [ e$ "# TestName Variant NumThreads   MinTime MedianTime MaxTime  Productivity1 Productivity2 Productivity3"
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
             , e$ "# Git_Depth: "  ++ show (length hashes)
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
    ls' <- sequence ls
    forM_ ls' $ \line -> do
      Strm.write (Just$ B.pack line) resultsOut
      Strm.write (Just$ B.pack line) logOut 
      Strm.write (Just$ B.pack line) stdOut
    return ()

 where 
   -- This is a hack for shell expanding inside a string:
   e :: String -> IO String
   e s =
     runSL ("echo \""++s++"\"")
     -- readCommand ("echo \""++s++"\"")
--     readProcess "echo" ["\""++s++"\""] ""


----------------------------------------------------------------------------------------------------
-- Main Script
----------------------------------------------------------------------------------------------------

-- | Command line flags.
data Flag = ParBench 
          | BinDir FilePath
          | NoRecomp | NoCabal | NoClean
          | ShowHelp
#ifdef FUSION_TABLES
          | FusionTables (Maybe TableId)
          | BenchsetName (String)
          | ClientID     String
          | ClientSecret String
#endif
  deriving (Eq,Ord,Show,Read)

-- | Command line options.
cli_options :: [OptDescr Flag]
cli_options = 
     [ Option ['p'] ["par"] (NoArg ParBench) 
       "Build benchmarks in parallel (run in parallel too if SHORTRUN=1)."
     , Option [] ["no-recomp"] (NoArg NoRecomp)
       "Don't perform any compilation of benchmark executables.  Implies -no-clean."
     , Option [] ["no-clean"] (NoArg NoClean)
       "Do not clean pre-existing executables before beginning."
     , Option [] ["no-cabal"] (NoArg NoCabal)
       "Build directly through GHC even if .cabal file is present."

     , Option ['h'] ["help"] (NoArg ShowHelp)
       "Show this help message and exit."
       
#ifdef FUSION_TABLES
     , Option [] ["fusion-tables"] (OptArg FusionTables "TABLEID")
       "enable fusion table upload.  Optionally set TABLEID; otherwise create/discover it."

     , Option [] ["name"]         (ReqArg BenchsetName "NAME") "Name for created/discovered fusion table."
     , Option [] ["clientid"]     (ReqArg ClientID "ID")     "Use (and cache) Google client ID"
     , Option [] ["clientsecret"] (ReqArg ClientSecret "STR") "Use (and cache) Google client secret"
#endif
       
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
  when (not (null errs && null args) || ShowHelp `elem` options) $ do
    putStrLn$ "Errors parsing command line options:" 
    mapM_ (putStr . ("   "++)) errs       
    putStrLn "\nUSAGE: [set ENV VARS] ./benchmark.hs [CMDLN OPTIONS]"    
    putStr$ usageInfo "\n CMDLN OPTIONS:" cli_options
    putStrLn$ usageStr    
    exitFailure

  -- HACK: with all the inter-machine syncing and different version
  -- control systems I run into permissions problems sometimes:
  system "chmod +x ./ntime* ./*.sh"

  conf@Config{benchlist,scheds,envs,stdOut,threadsettings} <- getConfig options
  
  hasMakefile <- doesFileExist "Makefile"
  cabalFile   <- runLines "ls *.cabal"
  let hasCabalFile = (cabalFile /= []) &&
                     not (NoCabal `elem` options)

  runReaderT 
    (do         
	log "Writing header for result data file:"
	printBenchrunHeader 
        
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
			s <- Set.toList (Set.intersection scheds (Set.fromList compatScheds)),
			t <- threadsettings,
                        e <- envs]

            allruns = listConfigs threadsettings 
            total = length allruns

            -- All that matters for compilation is nonthreaded (0) or threaded [1,inf)
            pruned = Set.toList $ Set.fromList $
                     -- Also ARGS and ENV can be ignored for compilation purposes:
                     map (\ (BenchRun { threads, sched, bench })
                              -> BenchRun { threads
                                          , sched
                                          , bench=bench{ args=[] }
                                          , env=[]} ) $ 
                     listConfigs $
                     Set.toList $ Set.fromList $
                     map (\ x -> if x==0 then 0 else 1) threadsettings

        log$ "\n--------------------------------------------------------------------------------"
        log$ "Running all benchmarks for all thread settings in "++show threadsettings
        log$ "Running for all schedulers in: "++show (Set.toList scheds)
        log$ "Testing "++show total++" total configurations of "++ show (length benchlist)++" benchmarks"
        log$ "--------------------------------------------------------------------------------"

        if ParBench `elem` options then do
            unless rtsSupportsBoundThreads $ error "benchmark.hs was NOT compiled with -threaded.  Can't do --par."
        --------------------------------------------------------------------------------
        -- Parallel version:
            lift$ putStrLn$ "[!!!] Compiling in Parallel, numCapabilities="++show numCapabilities++" ... "
               
            when recomp $ liftIO$ do 
              when hasCabalFile (error "Currently, cabalized build does not support parallelism!")


              -- This uses numCapabilities worker threads:
            
              -- This uses numCapabilities worker threads:
              (strms,barrier) <- parForM numCapabilities (zip [1..] pruned) $ \ outStrm (confnum,bench) -> do
                 -- let outStrm' = outStrm
                 outStrm' <- Strm.unlines outStrm
                 Strm.write (Just$ B.pack "[strm] CALLING compileOne...") outStrm'
--                 devnull <- Strm.handleToOutputStream =<< openFile "/dev/null" AppendMode
--                 let conf' = conf { stdOut = devnull }
                 let conf' = conf { stdOut = outStrm' } 
                 runReaderT (compileOne bench (confnum,length pruned)) conf'
                 Strm.write (Just$ B.pack "[strm] FINISHED with that compileOne...") outStrm'
                 return ()

                 
#if 0
              srcs <- Strm.fromList (zip (map show [1..]) strms)
              hydraPrint defaultHydraConf{deleteWhen=Never} srcs
#elif 1
              hydraPrintStatic defaultHydraConf (zip (map show [1..]) strms)
#elif 0
              strms2 <- mapM Strm.lines strms
              interleaved <- Strm.concurrentMerge strms2
              Strm.connect interleaved stdOut
--              Strm.toList interleaved
#else
              -- This version serializes the output one worker at a time:
              merged <- Strm.concatInputStreams strms
              -- Strm.connect (head strms) stdOut
              Strm.connect merged stdOut
#endif
              res <- barrier
              return ()

            Config{shortrun} <- ask
	    if shortrun then do
               lift$ putStrLn$ "[!!!] Running in Parallel..."
               error "benchmark.hs !!!Restore this part of the code"
	       -- (outputs,_) <- parForMTwoPhaseAsync (zip [1..] pruned) $ \ (confnum,bench) -> do
	       --    out@(BL _ ls3) <- forkWithBufferedLogs$ runOne bench (confnum,total)
	       --    return (ls3, forceBuffered out)
	       -- flushBuffered outputs 
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
                log$ "Found cabal file in top-level benchmark dir.  Using it to build all benchmarks: "++(head cabalFile)
                void invokeCabal
             else do              
                log$ "Not using Cabal.  Attempting to build the old fashioned way."             
                forM_ (zip [1..] pruned) $ \ (confnum,bench) -> 
                   compileOne bench (confnum,length pruned))

          forM_ (zip [1..] allruns) $ \ (confnum,bench) -> 
              runOne bench (confnum,total)

        do Config{logOut, resultsOut, stdOut} <- ask
           liftIO$ Strm.write Nothing logOut 
           liftIO$ Strm.write Nothing resultsOut 

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
forkIOH :: String -> IO () -> IO ThreadId
forkIOH who action = 
  forkIO $ handle (\ (e::SomeException) -> 
                   case fromException e of
                     Just ThreadKilled -> return ()
                     Nothing -> do 
		        hPutStrLn stderr$ "ERROR: "++who++": Got exception inside forked thread: "++show e
			tid <- readIORef main_threadid
			-- case maybhndls of 
			--   Nothing -> return ()
			--   Just (logBuf,resBuf,stdoutBuf) -> 
			--      do 
			-- 	hPutStrLn stderr "=========================="
			-- 	hPutStrLn stderr "Buffered contents for log:"
			-- 	hPutStrLn stderr "=========================="
			-- 	str <- peekBuffer logBuf
			-- 	hPutStrLn stderr (unlines str)
			-- 	return ()
			throwTo tid e
		  )
           action


-- | Runs a command through the OS shell and returns stdout split into
-- lines.
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


----------------------------------------------------------------------------------------------------
