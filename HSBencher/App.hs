{-# LANGUAGE BangPatterns, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleContexts #-}
{-# LANGUAGE CPP, OverloadedStrings, TupleSections #-}
--------------------------------------------------------------------------------
-- NOTE: This is best when compiled with "ghc -threaded"
-- However, ideally for real benchmarking runs we WANT the waitForProcess below block the whole process.
-- However^2, currently [2012.05.03] when running without threads I get errors like this:
--   benchmark.run: bench_hive.log: openFile: resource busy (file is locked)

--------------------------------------------------------------------------------
 

{- |
   
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

module HSBencher.App (defaultMain, defaultMainWithBechmarks) where 

----------------------------
-- Standard library imports
import Prelude hiding (log)
import Control.Applicative    
import Control.Concurrent
import Control.Monad.Reader
import Control.Exception (evaluate, handle, SomeException, throwTo, fromException, AsyncException(ThreadKilled))
import Debug.Trace
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Maybe (isJust, fromJust, catMaybes)
import qualified Data.Map as M
import Data.Word (Word64)
import Data.IORef
import Data.List (intercalate, sortBy, intersperse, isPrefixOf, tails, isInfixOf, delete)
import qualified Data.Set as Set
import Data.Version (versionBranch, versionTags)
import GHC.Conc (getNumProcessors)
import Numeric (showFFloat)
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
import qualified Data.ByteString.Char8 as B
import Text.Printf
import Text.PrettyPrint.GenericPretty (Out(doc))
-- import Text.PrettyPrint.HughesPJ (nest)
----------------------------
-- Additional libraries:

import qualified System.IO.Streams as Strm
import qualified System.IO.Streams.Concurrent as Strm
import qualified System.IO.Streams.Process as Strm
import qualified System.IO.Streams.Combinators as Strm

import UI.HydraPrint (hydraPrint, HydraConf(..), DeleteWinWhen(..), defaultHydraConf, hydraPrintStatic)
import Scripting.Parallel.ThreadPool (parForM)

#ifdef FUSION_TABLES
import Network.Google (retryIORequest)
import Network.Google.OAuth2 (getCachedTokens, refreshTokens, OAuth2Client(..), OAuth2Tokens(..))
import Network.Google.FusionTables (createTable, listTables, listColumns, insertRows,
                                    TableId, CellType(..), TableMetadata(..))
#endif

----------------------------
-- Self imports:

import HSBencher.Utils
import HSBencher.Logging
import HSBencher.Types
import HSBencher.Methods
import HSBencher.MeasureProcess 
import Paths_hsbencher (version) -- Thanks, cabal!

----------------------------------------------------------------------------------------------------


-- | USAGE
usageStr :: String
usageStr = unlines $
 [
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
   "",
#ifdef FUSION_TABLES   
   "     HSBENCHER_GOOGLE_CLIENTID, HSBENCHER_GOOGLE_CLIENTSECRET: if FusionTable upload is enabled, the",
   "               client ID and secret can be provided by env vars OR command line options. ",
#endif
   " ",
   "     ENVS='[[(\"KEY1\", \"VALUE1\")], [(\"KEY1\", \"VALUE2\")]]' to set",
   "     different configurations of environment variables to be set *at",
   "     runtime*. Useful for NUMA_TOPOLOGY, for example.  Note that this",
   "     can change multiple env variables in multiple distinct",
   "     configurations, with each configuration tested separately.",
   "",
   "   Additionally, this script will propagate any flags placed in the",
   "   environment variables $GHC_FLAGS and $GHC_RTS.  It will also use",
   "   $GHC or $CABAL, if available, to select the executable paths.", 
   "   ",
   "   Command line arguments take precedence over environment variables, if both apply."
 ]

----------------------------------------------------------------------------------------------------


gc_stats_flag :: String
gc_stats_flag = " -s " 
-- gc_stats_flag = " --machine-readable -t "

exedir :: String
exedir = "./bin"

--------------------------------------------------------------------------------

-- | Fill in "static" fields of a FusionTable row based on the `Config` data.
augmentTupleWithConfig :: Config -> [(String,String)] -> IO [(String,String)]
augmentTupleWithConfig Config{..} base = do
  ghcVer <- runSL$ ghc ++ " -V"
  let ghcVer' = collapsePrefix "The Glorious Glasgow Haskell Compilation System," "GHC" ghcVer
  datetime <- getCurrentTime
  uname    <- runSL "uname -a"
  lspci    <- runLines "lspci"
  whos     <- runLines "who"
  let runID = (hostname ++ "_" ++ show startTime)
  let (branch,revision,depth) = gitInfo
  return $ 
    addit "COMPILER"       ghcVer'   $
    addit "COMPILE_FLAGS"  ghc_flags $
    addit "RUNTIME_FLAGS"  ghc_RTS   $
    addit "HOSTNAME"       hostname $
    addit "RUNID"          runID $ 
    addit "DATETIME"       (show datetime) $
    addit "TRIALS"         (show trials) $
    addit "ENV_VARS"       (show envs) $
    addit "BENCH_VERSION"  (show$ snd benchversion) $
    addit "BENCH_FILE"     (fst benchversion) $
    addit "UNAME"          uname $
--    addit "LSPCI"          (unlines lspci) $
    addit "GIT_BRANCH"     branch   $
    addit "GIT_HASH"       revision $
    addit "GIT_DEPTH"      (show depth) $
    addit "WHO"            (unlines whos) $ 
    base
  where
    addit :: String -> String -> [(String,String)] -> [(String,String)]
    addit key val als =
      case lookup key als of
        Just b -> error$"augmentTupleWithConfig: cannot add field "++key++", already present!: "++b
        Nothing -> (key,val) : als

-- Retrieve the (default) configuration from the environment, it may
-- subsequently be tinkered with.  This procedure should be idempotent.
getConfig :: [Flag] -> [Benchmark2] -> IO Config
getConfig cmd_line_options benches = do
  hostname <- runSL$ "hostname -s"
  t0 <- getCurrentTime
  let startTime = round (utcTimeToPOSIXSeconds t0)
  env      <- getEnvironment

  -- There has got to be a simpler way!
  branch   <- runSL  "git name-rev --name-only HEAD"
  revision <- runSL  "git rev-parse HEAD"
  -- Note that this will NOT be newline-terminated:
  hashes   <- runLines "git log --pretty=format:'%H'"

  let       
      -- Read an ENV var with default:
      get v x = case lookup v env of 
		  Nothing -> x
		  Just  s -> s

--      benchF = get "BENCHLIST" "benchlist.txt"
      logFile = "bench_" ++ hostname ++ ".log"
      resultsFile = "results_" ++ hostname ++ ".dat"      
      shortrun = strBool (get "SHORTRUN"  "0")
  let scheds = case get "SCHEDS" "" of 
		"" -> defaultSchedSet
		s  -> Set.fromList (map read (words s))

  case get "GENERIC" "" of 
    "" -> return ()
    s  -> error$ "GENERIC env variable not handled yet.  Set to: " ++ show s
  
  maxthreads <- getNumProcessors

  backupResults resultsFile logFile

  rhnd <- openFile resultsFile WriteMode 
  lhnd <- openFile logFile     WriteMode

  hSetBuffering rhnd NoBuffering
  hSetBuffering lhnd NoBuffering  
  
  resultsOut <- Strm.unlines =<< Strm.handleToOutputStream rhnd
  logOut     <- Strm.unlines =<< Strm.handleToOutputStream lhnd
  stdOut     <- Strm.unlines Strm.stdout
      
  let -- Messy way to extract the benchlist version:
      -- ver = case filter (isInfixOf "ersion") (lines benchstr) of 
      --         (h:_t) -> read $ (\ (h:_)->h) $ filter isNumber (words h)
      --         []    -> 0
      -- This is our starting point BEFORE processing command line flags:
      base_conf = Config 
           { hostname, startTime, scheds, shortrun
           , benchsetName = Nothing
	   , ghc        =       get "GHC"       "ghc"
	   , cabalPath  =       get "CABAL"     "cabal"
           , ghc_pkg    =       get "GHC_PKG"   "ghc-pkg"
	   , ghc_RTS    =       get "GHC_RTS"   ("-qa " ++ gc_stats_flag) -- Default RTS flags.
  	   , ghc_flags  = (get "GHC_FLAGS" (if shortrun then "" else "-O2"))
	                  ++ " -rtsopts" -- Always turn on rts opts.
	   , trials         = read$ get "TRIALS"    "1"
--	   , benchlist      = parseBenchList benchstr
--	   , benchversion   = (benchF, ver)
           , benchlist      = benches
	   , benchversion   = ("",0)
	   , maxthreads     = maxthreads
	   , threadsettings = parseIntList$ get "THREADS" (show maxthreads)
	   , keepgoing      = strBool (get "KEEPGOING" "0")
	   , resultsFile, logFile, logOut, resultsOut, stdOut         
--	   , outHandles     = Nothing
           , envs           = read $ get "ENVS" "[[]]"
           , gitInfo        = (trim branch, trim revision, length hashes)
           -- This is in priority order:                   
           , buildMethods   = [cabalMethod, makeMethod, ghcMethod]
           , doFusionUpload = False                              
#ifdef FUSION_TABLES
           , fusionTableID  = Nothing 
           , fusionClientID     = lookup "HSBENCHER_GOOGLE_CLIENTID" env
           , fusionClientSecret = lookup "HSBENCHER_GOOGLE_CLIENTSECRET" env
#endif                              
	   }

  -- Process command line arguments to add extra cofiguration information:
  let 
#ifdef FUSION_TABLES
      doFlag (BenchsetName name) r     = r { benchsetName= Just name }
      doFlag (ClientID cid)   r = r { fusionClientID     = Just cid }
      doFlag (ClientSecret s) r = r { fusionClientSecret = Just s }
      doFlag (FusionTables m) r = 
         let r2 = r { doFusionUpload = True } in
         case m of 
           Just tid -> r2 { fusionTableID = Just tid }
           Nothing -> r2
#endif
      doFlag (CabalPath p) r = r { cabalPath=p }
      doFlag (GHCPath   p) r = r { ghc=p }
      -- Ignored options:
      doFlag ShowHelp r = r
      doFlag ShowVersion r = r
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
                  case (fusionClientID conf, fusionClientSecret conf) of
                    (Just cid, Just sec ) -> do
                      let auth = OAuth2Client { clientId=cid, clientSecret=sec }
                      tid <- runReaderT (getTableId auth name) conf
                      return conf{fusionTableID= Just tid}
                    (_,_) -> error "When --fusion-upload is activated --clientid and --clientsecret are required (or equiv ENV vars)"
#else
  let finalconf = conf      
#endif         
--  runReaderT (log$ "Read list of benchmarks/parameters from: "++benchF) finalconf
  return finalconf



-- | Remove RTS options that are specific to -threaded mode.
pruneThreadedOpts :: [String] -> [String]
pruneThreadedOpts = filter (`notElem` ["-qa", "-qb"])

  
--------------------------------------------------------------------------------
-- Error handling
--------------------------------------------------------------------------------

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
invokeCabal :: BenchM Bool
invokeCabal = do
  Config{ghc, ghc_pkg, ghc_flags, scheds, cabalPath} <- ask  
  bs <- forM (Set.toList scheds) $ \sched -> do
          let schedflag = schedToCabalFlag sched
              cmd = unwords [ cabalPath++" install"
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
          check False code ("ERROR, "++my_name++": cabal-based compilation failed.")
  
  return $ all id bs

-- | Build a single benchmark in a single configuration.
compileOne :: Benchmark2 -> [ParamSetting] -> BenchM Bool
compileOne Benchmark2{target=testPath,cmdargs} cconf = do
  Config{ghc, ghc_flags, shortrun, resultsOut, stdOut, buildMethods} <- ask

  let (diroffset,testRoot) = splitFileName testPath
      iterNum = 999
      totalIters = 999

  log$ "\n--------------------------------------------------------------------------------"
  log$ "  Compiling Config "++show iterNum++" of "++show totalIters++
       ": "++testRoot++" (args \""++unwords cmdargs++"\") confID "++
       (show$ makeBuildID$ toCompileFlags cconf)
  log$ "--------------------------------------------------------------------------------\n"


  matches <- lift$ 
             filterM (fmap isJust . (`filePredCheck` testPath) . canBuild) buildMethods 
  when (null matches) $ do
       log$ "ERROR, no build method matches path: "++testPath
       lift$ exitFailure     
  log$ printf "Found %d methods that can handle %s: %s" 
         (length matches) testPath (show$ map methodName matches)
  when (length matches > 1) $
    log$ "WARNING: resolving ambiguity, picking method: "++(methodName$ head matches)
  liftIO exitSuccess

  error$ "FINISHME - compileOne: "++show cconf

{-
-- | Build a single benchmark in a single configuration WITHOUT cabal.
compileOne :: BenchRun -> (Int,Int) -> BenchM Bool
compileOne br@(BenchRun { threads=numthreads
                        , sched
                        , bench=(Benchmark testPath _ args_)
                        }) 
	      (iterNum,totalIters) = 
  do Config{ghc, ghc_flags, shortrun, resultsOut, stdOut, buildMethods} <- ask

     uid :: Word64 <- lift$ randomIO
     let flags_ = case numthreads of
		   0 -> ghc_flags
		   _ -> ghc_flags++" -threaded"
	 flags = flags_ ++ " -fforce-recomp -DPARSCHED=\""++ (schedToModule sched) ++ "\""         

         args = if shortrun then shortArgs args_ else args_

---

     log$"First, creating a directory for intermediate compiler files: "++outdir
     code1 <- lift$ system$ "mkdir -p "++outdir
     code2 <- lift$ system$ "mkdir -p "++exedir
     check False code1 ("ERROR, "++my_name++": making compiler temp dir failed.")
     check False code2 ("ERROR, "++my_name++": making compiler temp dir failed.")

---

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

	 check False code ("ERROR, "++my_name++": compilation failed.")

     -- else if (d && mf && diroffset /= ".") then do
     --    log " ** Benchmark appears in a subdirectory with Makefile.  NOT supporting Makefile-building presently."
     --    error "No makefile-based builds supported..."
     else do 
	log$ "ERROR, "++my_name++": File does not exist: "++hsfile
	lift$ exitFailure
-}



--------------------------------------------------------------------------------
-- Running Benchmarks
--------------------------------------------------------------------------------

-- If the benchmark has already been compiled doCompile=False can be
-- used to skip straight to the execution.
runOne :: BenchRun -> (Int,Int) -> BenchM ()
runOne br@(BenchRun { threads=numthreads
                    , sched
                    , bench=(Benchmark testPath _ args_)
                    , env=envVars })
          (iterNum,totalIters) = do
  conf@Config{..} <- ask
  let args = if shortrun then shortArgs args_ else args_
      (_,testRoot) = splitFileName testPath
  log$ "\n--------------------------------------------------------------------------------"
  log$ "  Running Config "++show iterNum++" of "++show totalIters++
       ": "++testRoot++" (args \""++unwords args++"\") scheduler "++show sched++
       "  threads "++show numthreads++" (Env="++show envVars++")"
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
      exeFile = exedir </> testRoot ++ uniqueSuffix br ++ ".exe"
  ----------------------------------------
  -- Now execute N trials:
  ----------------------------------------
  -- (One option woud be dynamic feedback where if the first one
  -- takes a long time we don't bother doing more trials.)
  nruns <- forM [1..trials] $ \ i -> do 
    log$ printf "Running trial %d of %d" i trials
    let cmdArgs = args++["+RTS"]++words rts++["-RTS"]
    log "------------------------------------------------------------"        
    log$ " Executing command: " ++ unwords (exeFile:cmdArgs)    
    SubProcess {wait,process_out,process_err} <-
      lift$ measureProcess
              CommandDescr{ exeFile, cmdArgs, envVars, timeout=Just 150, workingDir=Nothing }
    err2 <- lift$ Strm.map (B.append " [stderr] ") process_err
    both <- lift$ Strm.concurrentMerge [process_out, err2]
    mv <- echoStream (not shortrun) both
    lift$ takeMVar mv
    lift wait

  (t1,t2,t3,p1,p2,p3) <-
    if not (all didComplete nruns) then do
      log $ "\n >>> MIN/MEDIAN/MAX (TIME,PROD) -- got ERRORS: " ++show nruns
      return ("","","","","","")
    else do 
      -- Extract the min, median, and max:
      let sorted = sortBy (\ a b -> compare (realtime a) (realtime b)) nruns
          minR = head sorted
          maxR = last sorted
          medianR = sorted !! (length sorted `quot` 2)

      let ts@[t1,t2,t3]    = map (\x -> showFFloat Nothing x "")
                             [realtime minR, realtime medianR, realtime maxR]
          prods@[p1,p2,p3] = map mshow [productivity minR, productivity medianR, productivity maxR]
          mshow Nothing  = ""
          mshow (Just x) = showFFloat (Just 2) x "" 

      let 
          pads n s = take (max 1 (n - length s)) $ repeat ' '
          padl n x = pads n x ++ x 
          padr n x = x ++ pads n x

          -- These are really (time,prod) tuples, but a flat list of
          -- scalars is simpler and readable by gnuplot:
          formatted = (padl 15$ unwords $ ts)
                      ++"   "++ unwords prods -- prods may be empty!

      log $ "\n >>> MIN/MEDIAN/MAX (TIME,PROD) " ++ formatted

      logOn [ResultsFile]$ 
        printf "%s %s %s %s %s" (padr 35 testRoot)   (padr 20$ intercalate "_" args)
                                (padr 7$ show sched) (padr 3$ show numthreads) formatted
      return (t1,t2,t3,p1,p2,p3)
#ifdef FUSION_TABLES
  when doFusionUpload $ do
    let (Just cid, Just sec) = (fusionClientID, fusionClientSecret)
        authclient = OAuth2Client { clientId = cid, clientSecret = sec }
    -- FIXME: it's EXTREMELY inefficient to authenticate on every tuple upload:
    toks  <- liftIO$ getCachedTokens authclient
    let         
        tuple =          
          [("PROGNAME",testRoot),("ARGS", unwords args),("THREADS",show numthreads),
           ("MINTIME",t1),("MEDIANTIME",t2),("MAXTIME",t3),
           ("MINTIME_PRODUCTIVITY",p1),("MEDIANTIME_PRODUCTIVITY",p2),("MAXTIME_PRODUCTIVITY",p3),
           ("VARIANT", show sched)]
    tuple' <- liftIO$ augmentTupleWithConfig conf tuple
    let (cols,vals) = unzip tuple'
    log$ " [fusiontable] Uploading row with "++show (length cols)++
         " columns containing "++show (sum$ map length vals)++" characters of data"
    -- 
    -- FIXME: It's easy to blow the URL size; we need the bulk import version.
    stdRetry "insertRows" authclient toks $
      insertRows (B.pack$ accessToken toks) (fromJust fusionTableID) cols [vals]
    log$ " [fusiontable] Done uploading, run ID "++ (fromJust$ lookup "RUNID" tuple')
         ++ " date "++ (fromJust$ lookup "DATETIME" tuple')
--       [[testRoot, unwords args, show numthreads, t1,t2,t3, p1,p2,p3]]
    return ()           
#endif
  return ()     





-- defaultColumns =
--   ["Program","Args","Threads","Sched","Threads",
--    "MinTime","MedianTime","MaxTime", "MinTime_Prod","MedianTime_Prod","MaxTime_Prod"]

#ifdef FUSION_TABLES
resultsSchema :: [(String, CellType)]
resultsSchema =
  [ ("PROGNAME",STRING)
  , ("VARIANT",STRING)
  , ("ARGS",STRING)    
  , ("HOSTNAME",STRING)
  -- The run is identified by hostname_secondsSinceEpoch:
  , ("RUNID",STRING)
  , ("THREADS",NUMBER)
  , ("DATETIME",DATETIME)    
  , ("MINTIME", NUMBER)
  , ("MEDIANTIME", NUMBER)
  , ("MAXTIME", NUMBER)
  , ("MINTIME_PRODUCTIVITY", NUMBER)
  , ("MEDIANTIME_PRODUCTIVITY", NUMBER)
  , ("MAXTIME_PRODUCTIVITY", NUMBER)
  , ("ALLTIMES", STRING)
  , ("TRIALS", NUMBER)
  , ("COMPILER",STRING)
  , ("COMPILE_FLAGS",STRING)
  , ("RUNTIME_FLAGS",STRING)
  , ("ENV_VARS",STRING)
  , ("BENCH_VERSION", STRING)
  , ("BENCH_FILE", STRING)
--  , ("OS",STRING)
  , ("UNAME",STRING)
  , ("PROCESSOR",STRING)
  , ("TOPOLOGY",STRING)
  , ("GIT_BRANCH",STRING)
  , ("GIT_HASH",STRING)
  , ("GIT_DEPTH",NUMBER)
  , ("WHO",STRING)
  , ("ETC_ISSUE",STRING)
  , ("LSPCI",STRING)    
  , ("FULL_LOG",STRING)
  ]

-- | The standard retry behavior when receiving HTTP network errors.
stdRetry :: String -> OAuth2Client -> OAuth2Tokens -> IO a ->
            BenchM a
stdRetry msg client toks action = do
  conf <- ask
  let retryHook exn = runReaderT (do
        log$ " [fusiontable] Retrying during <"++msg++"> due to HTTPException: " ++ show exn
        log$ " [fusiontable] Retrying, but first, attempt token refresh..."
        -- QUESTION: should we retry the refresh itself, it is NOT inside the exception handler.
        -- liftIO$ refreshTokens client toks
        -- liftIO$ retryIORequest (refreshTokens client toks) (\_ -> return ()) [1,1]
        stdRetry "refresh tokens" client toks (refreshTokens client toks)
        return ()
                                 ) conf
  liftIO$ retryIORequest action retryHook [1,2,4,8,16,32,64]

-- | Get the table ID that has been cached on disk, or find the the table in the users
-- Google Drive, or create a new table if needed.
getTableId :: OAuth2Client -> String -> BenchM TableId
getTableId auth tablename = do
  log$ " [fusiontable] Fetching access tokens, client ID/secret: "++show (clientId auth, clientSecret auth)
  toks      <- liftIO$ getCachedTokens auth
  log$ " [fusiontable] Retrieved: "++show toks
  let atok  = B.pack $ accessToken toks
  allTables <- stdRetry "listTables" auth toks $ listTables atok
  log$ " [fusiontable] Retrieved metadata on "++show (length allTables)++" tables"

  case filter (\ t -> tab_name t == tablename) allTables of
    [] -> do log$ " [fusiontable] No table with name "++show tablename ++" found, creating..."
             TableMetadata{tab_tableId} <- stdRetry "createTable" auth toks $
                                           createTable atok tablename resultsSchema
             log$ " [fusiontable] Table created with ID "++show tab_tableId
             return tab_tableId
    [t] -> do log$ " [fusiontable] Found one table with name "++show tablename ++", ID: "++show (tab_tableId t)
              return (tab_tableId t)
    ls  -> error$ " More than one table with the name '"++show tablename++"' !\n "++show ls
#endif


------------------------------------------------------------

-- Helper for launching processes with logging and error checking
-----------------------------------------------------------------
-- [2012.05.03] HSH has been causing no end of problems in the
-- subprocess-management department.  Here we instead use the
-- underlying createProcess library function:
runCmdWithEnv :: Bool -> [(String, String)] -> String
              -> BenchM (String, ExitCode)
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
  check keepgoing code ("ERROR, "++my_name++": command \""++cmd++"\" failed with code "++ show code)
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
echoThread :: Bool -> Handle -> BenchM (MVar String)
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

-- | Create a thread that echos the contents of stdout/stderr InputStreams (lines) to
-- the appropriate places.
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
--            logOn (if echoStdout then [LogFile, StdOut] else [LogFile]) (B.unpack ln)
            lift$ B.putStrLn ln
            echoloop mv


--------------------------------------------------------------------------------

-- TODO: Remove this hack.
whichVariant :: String -> String
whichVariant "benchlist.txt"        = "desktop"
whichVariant "benchlist_server.txt" = "server"
whichVariant "benchlist_laptop.txt" = "laptop"
whichVariant _                      = "unknown"

-- | Write the results header out stdout and to disk.
printBenchrunHeader :: BenchM ()
printBenchrunHeader = do
  Config{ghc, trials, ghc_flags, ghc_RTS, maxthreads,
         logOut, resultsOut, stdOut, benchversion, shortrun, gitInfo=(branch,revision,depth) } <- ask
  liftIO $ do   
--    let (benchfile, ver) = benchversion
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
--             , e$ "# Benchmarks_File: " ++ benchfile
--             , e$ "# Benchmarks_Variant: " ++ if shortrun then "SHORTRUN" else whichVariant benchfile
--             , e$ "# Benchmarks_Version: " ++ show ver
             , e$ "# Git_Branch: " ++ branch
             , e$ "# Git_Hash: "   ++ revision
             , e$ "# Git_Depth: "  ++ show depth
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
          | CabalPath String
          | GHCPath String
          | ShowHelp | ShowVersion
#ifdef FUSION_TABLES
          | FusionTables (Maybe TableId)
          | BenchsetName (String)
          | ClientID     String
          | ClientSecret String
#endif
  deriving (Eq,Ord,Show,Read)

-- | Command line options.
core_cli_options :: (String, [OptDescr Flag])
core_cli_options = 
     ("\n Command Line Options:",
      [ Option ['p'] ["par"] (NoArg ParBench) 
        "Build benchmarks in parallel (run in parallel too if SHORTRUN=1)."
      , Option [] ["no-recomp"] (NoArg NoRecomp)
        "Don't perform any compilation of benchmark executables.  Implies -no-clean."
      , Option [] ["no-clean"] (NoArg NoClean)
        "Do not clean pre-existing executables before beginning."
      , Option [] ["no-cabal"] (NoArg NoCabal)
        "Build directly through GHC even if .cabal file is present."

      , Option [] ["with-cabal-install"] (ReqArg CabalPath "PATH")
        "Set the version of cabal-install to use, default 'cabal'."
      , Option [] ["with-ghc"] (ReqArg GHCPath "PATH")
        "Set the path of the ghc compiler, default 'ghc'."

      , Option ['h'] ["help"] (NoArg ShowHelp)
        "Show this help message and exit."

      , Option ['V'] ["version"] (NoArg ShowVersion)
        "Show the version and exit"
     ])

all_cli_options :: [(String, [OptDescr Flag])]
all_cli_options = [core_cli_options]
#ifdef FUSION_TABLES
                ++ [fusion_cli_options]

fusion_cli_options :: (String, [OptDescr Flag])
fusion_cli_options =
  ("\n Fusion Table Options:",
      [ Option [] ["fusion-upload"] (OptArg FusionTables "TABLEID")
        "enable fusion table upload.  Optionally set TABLEID; otherwise create/discover it."

      , Option [] ["name"]         (ReqArg BenchsetName "NAME") "Name for created/discovered fusion table."
      , Option [] ["clientid"]     (ReqArg ClientID "ID")     "Use (and cache) Google client ID"
      , Option [] ["clientsecret"] (ReqArg ClientSecret "STR") "Use (and cache) Google client secret"
      ])
#endif


-- | Global variable holding the main thread id.
main_threadid :: IORef ThreadId
main_threadid = unsafePerformIO$ newIORef (error "main_threadid uninitialized")


-- | TODO: Eventually this will be parameterized.
defaultMain :: IO ()
defaultMain = do
  error "FINISHME: defaultMain requires reading benchmark list from a file.  Implement it!"
  defaultMainWithBechmarks undefined

defaultMainWithBechmarks :: [Benchmark2] -> IO ()
defaultMainWithBechmarks benches = do  
  id <- myThreadId
  writeIORef main_threadid id

  cli_args <- getArgs
  let (options,args,errs) = getOpt Permute (concat$ map snd all_cli_options) cli_args

  when (ShowVersion `elem` options) $ do
    putStrLn$ "hsbencher version "++
      (concat$ intersperse "." $ map show $ versionBranch version) ++
      (unwords$ versionTags version)
    exitSuccess 
      
  when (not (null errs && null args) || ShowHelp `elem` options) $ do
    unless (ShowHelp `elem` options) $
      putStrLn$ "Errors parsing command line options:"
    mapM_ (putStr . ("   "++)) errs       
    putStrLn$ "\nUSAGE: [set ENV VARS] "++my_name++" [CMDLN OPTIONS]"
    mapM putStr (map (uncurry usageInfo) all_cli_options)
    putStrLn$ usageStr
    if (ShowHelp `elem` options) then exitSuccess else exitFailure

  conf@Config{scheds,envs,benchlist,stdOut,threadsettings} <- getConfig options benches
        
  hasMakefile <- doesFileExist "Makefile"
  cabalFile   <- runLines "ls *.cabal"
  let hasCabalFile = (cabalFile /= []) &&
                     not (NoCabal `elem` options)

  runReaderT 
    (do         
	log "Writing header for result data file:"
	printBenchrunHeader 

        let recomp  = NoRecomp `notElem` options
{-     
            doclean = (NoCabal `notElem` options) && recomp
        when doclean $ 
          let cleanit cmd = when (NoClean `notElem` options) $ do
                log$ "Before testing, first '"++ cmd ++"' for hygiene."
                code <- lift$ system$ cmd++" &> clean_output.tmp"
                check False code "ERROR: cleaning failed."
	        log " -> Cleaning Succeeded."
                liftIO$ removeFile "clean_output.tmp"
          in      if hasMakefile  then cleanit "make clean"
             else if hasCabalFile then cleanit (cabalPath conf++" clean")
             else    return ()
-}
        unless recomp $ log "[!!!] Skipping benchmark recompilation!"

        let
            benches' = map (\ b -> b { configs= compileOptsOnly (configs b) })
                       benchlist
            cfgs = map (enumerateBenchSpace . configs) benches'
            allcompiles = concat $
                          zipWith (\ b cs -> map (b,) cs) benches' cfgs
            
            total = sum $ map length cfgs
            
        log$ "\n--------------------------------------------------------------------------------"
        log$ "Running all benchmarks for all settings ..."
        log$ "Compiling: "++show total++" total configurations of "++ show (length benchlist)++" benchmarks"
        let indent n str = unlines $ map (replicate n ' ' ++) $ lines str
            printloop _ [] = return ()
            printloop mp (Benchmark2{target,cmdargs,configs} :tl) = do
              log$ " * Benchmark/args: "++target++" "++show cmdargs
              case M.lookup configs mp of
                Nothing -> log$ indent 4$ show$ doc configs
                Just trg0 -> log$ "   ...same config space as "++show trg0
              printloop (M.insertWith (\ _ x -> x) configs target mp) tl
--        log$ "Benchmarks/compile options: "++show (doc benches')              
        printloop M.empty benches'
        log$ "--------------------------------------------------------------------------------"

        if ParBench `elem` options then do
            unless rtsSupportsBoundThreads $ error (my_name++" was NOT compiled with -threaded.  Can't do --par.")
     {-            
        --------------------------------------------------------------------------------
        -- Parallel version:
            numProcs <- liftIO getNumProcessors
            lift$ putStrLn$ "[!!!] Compiling in Parallel, numProcessors="++show numProcs++" ... "
               
            when recomp $ liftIO$ do 
              when hasCabalFile (error "Currently, cabalized build does not support parallelism!")
            
              (strms,barrier) <- parForM numProcs (zip [1..] pruned) $ \ outStrm (confnum,bench) -> do
                 outStrm' <- Strm.unlines outStrm
                 let conf' = conf { stdOut = outStrm' } 
                 runReaderT (compileOne bench (confnum,length pruned)) conf'
                 return ()
              catParallelOutput strms stdOut
              res <- barrier
              return ()

            Config{shortrun,doFusionUpload} <- ask
	    if shortrun && not doFusionUpload then liftIO$ do
               putStrLn$ "[!!!] Running in Parallel..."              
               (strms,barrier) <- parForM numProcs (zip [1..] pruned) $ \ outStrm (confnum,bench) -> do
                  outStrm' <- Strm.unlines outStrm
                  let conf' = conf { stdOut = outStrm' }
                  runReaderT (runOne bench (confnum,total)) conf'
               catParallelOutput strms stdOut
               _ <- barrier
               return ()
	     else do
               -- Non-shortrun's NEVER run multiple benchmarks at once:
	       forM_ (zip [1..] allruns) $ \ (confnum,bench) -> 
		    runOne bench (confnum,total)
               return ()
-}
        else do
        --------------------------------------------------------------------------------
        -- Serial version:
          when recomp $ 
            forM_ (zip benches' cfgs) $ \ (bench, allcfgs) -> 
              forM_ allcfgs $ \ cfg -> 
                compileOne bench cfg -- confnum -- (confnum,length pruned)

          -- forM_ (zip [1..] allruns) $ \ (confnum,bench) -> 
          --     runOne bench (confnum,total)

{-
        do Config{logOut, resultsOut, stdOut} <- ask
           liftIO$ Strm.write Nothing logOut 
           liftIO$ Strm.write Nothing resultsOut 
-}
        log$ "\n--------------------------------------------------------------------------------"
        log "  Finished with all test configurations."
        log$ "--------------------------------------------------------------------------------"
	liftIO$ exitSuccess
    )
    conf


-- Several different options for how to display output in parallel:
catParallelOutput :: [Strm.InputStream B.ByteString] -> Strm.OutputStream B.ByteString -> IO ()
catParallelOutput strms stdOut = do 
 case 4 of
   -- First option is to create N window panes immediately.
   1 -> do
           hydraPrintStatic defaultHydraConf (zip (map show [1..]) strms)
   2 -> do
           srcs <- Strm.fromList (zip (map show [1..]) strms)
           hydraPrint defaultHydraConf{deleteWhen=Never} srcs
   -- This version interleaves their output lines (ugly):
   3 -> do 
           strms2 <- mapM Strm.lines strms
           interleaved <- Strm.concurrentMerge strms2
           Strm.connect interleaved stdOut
   -- This version serializes the output one worker at a time:           
   4 -> do
           strms2 <- mapM Strm.lines strms
           merged <- Strm.concatInputStreams strms2
           -- Strm.connect (head strms) stdOut
           Strm.connect merged stdOut


----------------------------------------------------------------------------------------------------
-- *                                 GENERIC HELPER ROUTINES                                      
----------------------------------------------------------------------------------------------------

-- These should go in another module.......

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

collapsePrefix :: String -> String -> String -> String
collapsePrefix old new str =
  if isPrefixOf old str
  then new ++ drop (length old) str
  else str  

didComplete RunCompleted{} = True
didComplete _              = False

-- TODO: grab this from the command line arguments:
my_name :: String
my_name = "hsbencher"

----------------------------------------------------------------------------------------------------
