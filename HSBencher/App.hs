{-# LANGUAGE BangPatterns, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleContexts #-}
{-# LANGUAGE CPP, OverloadedStrings, TupleSections #-}
--------------------------------------------------------------------------------
-- NOTE: This is best when compiled with "ghc -threaded"
-- However, ideally for real benchmarking runs we WANT the waitForProcess below block the whole process.
-- However^2, currently [2012.05.03] when running without threads I get errors like this:
--   benchmark.run: bench_hive.log: openFile: resource busy (file is locked)

--------------------------------------------------------------------------------

-- Disabling some stuff until we can bring it back up after the big transition [2013.05.28]:
#define DISABLED

{- | The Main module defining the HSBencher driver.
-}

module HSBencher.App
       (defaultMainWithBechmarks, defaultMainModifyConfig,
        Flag(..), all_cli_options)
       where 

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
import System.Environment (getArgs, getEnv, getEnvironment)
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
import HSBencher.Fusion
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
import HSBencher.Config
import HSBencher.Methods
import HSBencher.MeasureProcess 
import Paths_hsbencher (version) -- Thanks, cabal!

----------------------------------------------------------------------------------------------------


-- | USAGE
usageStr :: String
usageStr = unlines $
 [
   "   ",         
   " Many of these options can redundantly be set either when the benchmark driver is run,",
   " or in the benchmark descriptions themselves.  E.g. --with-ghc is just for convenience.",
   "\n ENV VARS:",
   "   These environment variables control the behavior of the benchmark script:",
   "",
#ifndef DISABLED
   "     SHORTRUN=1 to get a shorter run for testing rather than benchmarking.",
   "",
   "     THREADS=\"1 2 4\" to run with # threads = 1, 2, or 4.",
   "",
   "     BENCHLIST=foo.txt to select the benchmarks and their arguments",
   "               (uses benchlist.txt by default)",
   "",
   "     SCHEDS=\"Trace Direct Sparks\" -- Restricts to a subset of schedulers.",
   "",
   "     GENERIC=1 to go through the generic (type class) monad par",
   "               interface instead of using each scheduler directly",
   "",
   "     KEEPGOING=1 to keep going after the first error.",
   "",
   "     TRIALS=N to control the number of times each benchmark is run.",
   "",
#endif
#ifdef FUSION_TABLES   
   "     HSBENCHER_GOOGLE_CLIENTID, HSBENCHER_GOOGLE_CLIENTSECRET: if FusionTable upload is enabled, the",
   "               client ID and secret can be provided by env vars OR command line options. ",
#endif
   " ",
#ifndef DISABLED
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
#endif
   "   Command line arguments take precedence over environment variables, if both apply."
 ]

----------------------------------------------------------------------------------------------------


gc_stats_flag :: String
gc_stats_flag = " -s " 
-- gc_stats_flag = " --machine-readable -t "

exedir :: String
exedir = "./bin"

--------------------------------------------------------------------------------

-- | Remove RTS options that are specific to -threaded mode.
pruneThreadedOpts :: [String] -> [String]
pruneThreadedOpts = filter (`notElem` ["-qa", "-qb"])

  
--------------------------------------------------------------------------------
-- Error handling
--------------------------------------------------------------------------------

path :: [FilePath] -> FilePath
path [] = ""
path ls = foldl1 (</>) ls

--------------------------------------------------------------------------------
-- Compiling Benchmarks
--------------------------------------------------------------------------------

-- | Build a single benchmark in a single configuration.
compileOne :: (Int,Int) -> Benchmark DefaultParamMeaning -> [(DefaultParamMeaning,ParamSetting)] -> BenchM BuildResult
compileOne (iterNum,totalIters) Benchmark{target=testPath,cmdargs} cconf = do
  Config{shortrun, resultsOut, stdOut, buildMethods, pathRegistry, doClean} <- ask

  let (diroffset,testRoot) = splitFileName testPath
      flags = toCompileFlags cconf
      paths = toCmdPaths     cconf
      bldid = makeBuildID testPath flags
  log  "\n--------------------------------------------------------------------------------"
  log$ "  Compiling Config "++show iterNum++" of "++show totalIters++
       ": "++testRoot++" (args \""++unwords cmdargs++"\") confID "++ show bldid
  log  "--------------------------------------------------------------------------------\n"

  matches <- lift$ 
             filterM (fmap isJust . (`filePredCheck` testPath) . canBuild) buildMethods 
  when (null matches) $ do
       logT$ "ERROR, no build method matches path: "++testPath
       lift exitFailure     
  logT$ printf "Found %d methods that can handle %s: %s" 
         (length matches) testPath (show$ map methodName matches)
  let BuildMethod{methodName,clean,compile,concurrentBuild} = head matches
  when (length matches > 1) $
    logT$ " WARNING: resolving ambiguity, picking method: "++methodName

  let pathR = (M.union (M.fromList paths) pathRegistry)
  
  when doClean $ clean pathR bldid testPath

  -- Prefer the benchmark-local path definitions:
  x <- compile pathR bldid flags testPath
  logT$ "Compile finished, result: "++ show x
  return x
  

--------------------------------------------------------------------------------
-- Running Benchmarks
--------------------------------------------------------------------------------

-- If the benchmark has already been compiled doCompile=False can be
-- used to skip straight to the execution.
runOne :: (Int,Int) -> BuildID -> BuildResult -> Benchmark DefaultParamMeaning -> [(DefaultParamMeaning,ParamSetting)] -> BenchM ()
runOne (iterNum, totalIters) bldid bldres Benchmark{target=testPath, cmdargs=args_} runconfig = do       
  let numthreads = foldl (\ acc (x,_) ->
                           case x of
                             Threads n -> n
                             _         -> acc)
                   0 runconfig
      sched      = foldl (\ acc (x,_) ->
                           case x of
                             Variant s -> s
                             _         -> acc)
                   "none" runconfig
      
  let runFlags = toRunFlags runconfig
      envVars  = toEnvVars  runconfig
  conf@Config{..} <- ask

  ----------------------------------------
  -- (1) Gather contextual information
  ----------------------------------------  
  let args = if shortrun then shortArgs args_ else args_
      fullargs = if argsBeforeFlags 
                 then args ++ runFlags
                 else runFlags ++ args
      testRoot = fetchBaseName testPath
  log$ "\n--------------------------------------------------------------------------------"
  log$ "  Running Config "++show iterNum++" of "++show totalIters ++": "++testPath
--       "  threads "++show numthreads++" (Env="++show envVars++")"
  log$ nest 3 $ show$ doc$ map snd runconfig
  log$ "--------------------------------------------------------------------------------\n"
  pwd <- lift$ getCurrentDirectory
  logT$ "(In directory "++ pwd ++")"

  logT$ "Next run 'who', reporting users other than the current user.  This may help with detectivework."
--  whos <- lift$ run "who | awk '{ print $1 }' | grep -v $USER"
  whos <- lift$ runLines$ "who"
  let whos' = map ((\ (h:_)->h) . words) whos
  user <- lift$ getEnv "USER"
  logT$ "Who_Output: "++ unwords (filter (/= user) whos')

  -- If numthreads == 0, that indicates a serial run:

  ----------------------------------------
  -- (2) Now execute N trials:
  ----------------------------------------
  -- (One option woud be dynamic feedback where if the first one
  -- takes a long time we don't bother doing more trials.)
  nruns <- forM [1..trials] $ \ i -> do 
    log$ printf "  Running trial %d of %d" i trials
    log "  ------------------------"
    let (timeHarvest,ph) = harvesters
        prodHarvest = case ph of
                       Nothing -> nullHarvester
                       Just h  -> h
        doMeasure cmddescr = do
          SubProcess {wait,process_out,process_err} <-
            lift$ measureProcess timeHarvest prodHarvest cmddescr
          err2 <- lift$ Strm.map (B.append " [stderr] ") process_err
          both <- lift$ Strm.concurrentMerge [process_out, err2]
          mv <- echoStream (not shortrun) both
          lift$ takeMVar mv
          x <- lift wait
          return x
    case bldres of
      StandAloneBinary binpath -> do
        -- NOTE: For now allowing rts args to include things like "+RTS -RTS", i.e. multiple tokens:
        let command = binpath++" "++unwords fullargs 
        logT$ " Executing command: " ++ command
        doMeasure CommandDescr{ command=ShellCommand command, envVars, timeout=Just defaultTimeout, workingDir=Nothing }
      RunInPlace fn -> do
--        logT$ " Executing in-place benchmark run."
        let cmd = fn fullargs envVars
        logT$ " Generated in-place run command: "++show cmd
        doMeasure cmd

  ------------------------------------------
  -- (3) Produce output to the right places:
  ------------------------------------------
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
                                (padr 8$ sched) (padr 3$ show numthreads) formatted

      let result =
            emptyBenchmarkResult
            { _PROGNAME = testRoot
            , _VARIANT  = show sched
            , _ARGS     = args
            , _THREADS  = numthreads
            , _MINTIME    =  realtime minR
            , _MEDIANTIME =  realtime medianR
            , _MAXTIME    =  realtime maxR
            , _MINTIME_PRODUCTIVITY    = productivity minR
            , _MEDIANTIME_PRODUCTIVITY = productivity medianR
            , _MAXTIME_PRODUCTIVITY    = productivity maxR
            , _ALLTIMES      =  unwords$ map (show . realtime) nruns
            , _TRIALS        =  trials
            }
      result' <- liftIO$ augmentResultWithConfig conf result
#ifdef FUSION_TABLES
      when doFusionUpload $ uploadBenchResult result'
#endif      
      return (t1,t2,t3,p1,p2,p3)
      
  return ()     


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
  Config{trials, maxthreads, pathRegistry, 
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
             , e$ "# "                                                                
             , e$ "# Running each test for "++show trials++" trial(s)."
--             , e$ "# Benchmarks_File: " ++ benchfile
--             , e$ "# Benchmarks_Variant: " ++ if shortrun then "SHORTRUN" else whichVariant benchfile
--             , e$ "# Benchmarks_Version: " ++ show ver
             , e$ "# Git_Branch: " ++ branch
             , e$ "# Git_Hash: "   ++ revision
             , e$ "# Git_Depth: "  ++ show depth
             -- , e$ "# Using the following settings from environment variables:" 
             -- , e$ "#  ENV BENCHLIST=$BENCHLIST"
             -- , e$ "#  ENV THREADS=   $THREADS"
             -- , e$ "#  ENV TRIALS=    $TRIALS"
             -- , e$ "#  ENV SHORTRUN=  $SHORTRUN"
             -- , e$ "#  ENV KEEPGOING= $KEEPGOING"
             -- , e$ "#  ENV GHC=       $GHC"
             -- , e$ "#  ENV GHC_FLAGS= $GHC_FLAGS"
             -- , e$ "#  ENV GHC_RTS=   $GHC_RTS"
             -- , e$ "#  ENV ENVS=      $ENVS"
             , e$ "#  Path registry: "++show pathRegistry
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


-- | TODO: Eventually this will be parameterized.
defaultMain :: IO ()
defaultMain = do
  --      benchF = get "BENCHLIST" "benchlist.txt"
--  putStrLn$ hsbencher_tag ++ " Reading benchmark list from file: "
  error "FINISHME: defaultMain requires reading benchmark list from a file.  Implement it!"
--  defaultMainWithBechmarks undefined

-- | In this version, user provides a list of benchmarks to run, explicitly.
defaultMainWithBechmarks :: [Benchmark DefaultParamMeaning] -> IO ()
defaultMainWithBechmarks benches = do
  defaultMainModifyConfig (\ conf -> conf{ benchlist=benches })

-- | An even more flexible version allows the user to install a hook which modifies
-- the configuration just before bencharking begins.  All trawling of the execution
-- environment (command line args, environment variables) happens BEFORE the user
-- sees the configuration.
defaultMainModifyConfig :: (Config -> Config) -> IO ()
defaultMainModifyConfig modConfig = do    
  id <- myThreadId
  writeIORef main_threadid id

  cli_args <- getArgs
  let (options,args,errs) = getOpt Permute (concat$ map snd all_cli_options) cli_args
  let recomp  = NoRecomp `notElem` options
  
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

  conf0 <- getConfig options []
  let conf1@Config{envs,benchlist,stdOut,threadsettings} = modConfig conf0

  hasMakefile <- doesFileExist "Makefile"
  cabalFile   <- runLines "ls *.cabal"
  let hasCabalFile = (cabalFile /= []) &&
                     not (NoCabal `elem` options)
  rootDir <- getCurrentDirectory  
  runReaderT 
    (do
        logT$"Beginning benchmarking, root directory: "++rootDir
        let globalBinDir = rootDir </> "bin"
        when recomp $ do
          logT$"Clearing any preexisting files in ./bin/"
          lift$ do
            -- runSimple "rm -f ./bin/*"
            -- Yes... it's posix dependent.  But right now I don't see a good way to
            -- delete the contents a dir without (1) following symlinks or (2) assuming
            -- either the unix package or unix shell support (rm).
            --- Ok, what the heck, deleting recursively:
            dde <- doesDirectoryExist globalBinDir
            when dde $ removeDirectoryRecursive globalBinDir
        lift$ createDirectoryIfMissing True globalBinDir 
     
	logT "Writing header for result data file:"
	printBenchrunHeader
     
        unless recomp $ log "[!!!] Skipping benchmark recompilation!"

        let
            benches' = map (\ b -> b { configs= compileOptsOnly (configs b) })
                       benchlist
            cccfgs = map (enumerateBenchSpace . configs) benches' -- compile configs
            cclengths = map length cccfgs
            totalcomps = sum cclengths
            
        log$ "\n--------------------------------------------------------------------------------"
        logT$ "Running all benchmarks for all settings ..."
        logT$ "Compiling: "++show totalcomps++" total configurations of "++ show (length benchlist)++" benchmarks"
        let indent n str = unlines $ map (replicate n ' ' ++) $ lines str
            printloop _ [] = return ()
            printloop mp (Benchmark{target,cmdargs,configs} :tl) = do
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
                  runReaderT (runOne bench (confnum,totalcomps)) conf'
               catParallelOutput strms stdOut
               _ <- barrier
               return ()
	     else do
               -- Non-shortrun's NEVER run multiple benchmarks at once:
	       forM_ (zip [1..] allruns) $ \ (confnum,bench) -> 
		    runOne bench (confnum,totalcomps)
               return ()
-}
        else do
        --------------------------------------------------------------------------------
        -- Serial version:
          -- TODO: make this a foldlM:
          let allruns = map (enumerateBenchSpace . configs) benchlist
              allrunsLens = map length allruns
              totalruns = sum allrunsLens
          let 
              -- Here we lazily compile benchmarks as they become required by run configurations.
              runloop :: Int 
                      -> M.Map BuildID (Int, Maybe BuildResult)
                      -> M.Map FilePath BuildID -- (S.Set ParamSetting)
                      -> [(Benchmark DefaultParamMeaning, [(DefaultParamMeaning,ParamSetting)])]
                      -> BenchM ()
              runloop _ _ _ [] = return ()
              runloop !iter !board !lastConfigured (nextrun:rest) = do
                -- lastConfigured keeps track of what configuration was last built in
                -- a directory that is used for `RunInPlace` builds.
                let (bench,params) = nextrun
                    ccflags = toCompileFlags params
                    bid = makeBuildID (target bench) ccflags
                case M.lookup bid board of 
                  Nothing -> error$ "HSBencher: Internal error: Cannot find entry in map for build ID: "++show bid
                  Just (ccnum, Nothing) -> do 
                    res  <- compileOne (ccnum,totalcomps) bench params                    
                    let board' = M.insert bid (ccnum, Just res) board
                        lastC' = M.insert (target bench) bid lastConfigured

                    -- runloop iter board' (nextrun:rest)
                    runOne (iter,totalruns) bid res bench params
                    runloop (iter+1) board' lastC' rest

                  Just (ccnum, Just bldres) -> 
                    let proceed = do runOne (iter,totalruns) bid bldres bench params
                                     runloop (iter+1) board lastConfigured rest 
                    in
                    case bldres of 
                      StandAloneBinary _ -> proceed
                      RunInPlace _ -> 
                        -- Here we know that some previous compile with the same BuildID inserted this here.
                        -- But the relevant question is whether some other config has stomped on it in the meantime.
                        case M.lookup (target bench) lastConfigured of 
                          Nothing -> error$"HSBencher: Internal error, RunInPlace in the board but not lastConfigured!: "
                                       ++(target bench)++ " build id "++show bid
                          Just bid2 ->
                           if bid == bid2 
                           then do logT$ "Skipping rebuild of in-place benchmark: "++bid
                                   proceed 
                           else runloop iter (M.insert bid (ccnum,Nothing) board) lastConfigured (nextrun:rest)

              initBoard _ [] acc = acc 
              initBoard !iter ((bench,params):rest) acc = 
                let bid = makeBuildID (target bench) $ toCompileFlags params 
                    base = fetchBaseName (target bench)
                    dfltdest = globalBinDir </> base ++"_"++bid in
                case M.lookup bid acc of
                  Just _  -> initBoard iter rest acc
                  Nothing -> 
                    let elm = if recomp 
                              then (iter, Nothing)
                              else (iter, Just (StandAloneBinary dfltdest))
                    in
                    initBoard (iter+1) rest (M.insert bid elm acc)

              zippedruns = (concat$ zipWith (\ b cfs -> map (b,) cfs) benchlist allruns)

          unless recomp $ logT$ "Recompilation disabled, assuming standalone binaries are in the expected places!"
          runloop 1 (initBoard 1 zippedruns M.empty) M.empty zippedruns

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
    conf1


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



collapsePrefix :: String -> String -> String -> String
collapsePrefix old new str =
  if isPrefixOf old str
  then new ++ drop (length old) str
  else str  

didComplete RunCompleted{} = True
didComplete _              = False

-- Shorthand for tagged version:
logT str = log$hsbencher_tag++str
hsbencher_tag = " [hsbencher] "

-- Compute a cut-down version of a benchmark's args list that will do
-- a short (quick) run.  The way this works is that benchmarks are
-- expected to run and do something quick if they are invoked with no
-- arguments.  (A proper benchmarking run, therefore, requires larger
-- numeric arguments be supplied.)
-- 
shortArgs :: [String] -> [String]
shortArgs _ls = []

-- shortArgs [] = []
-- DISABLING:
-- HOWEVER: there's a further hack here which is that leading
-- non-numeric arguments are considered qualitative (e.g. "monad" vs
-- "sparks") rather than quantitative and are not pruned by this
-- function.
-- shortArgs (h:tl) | isNumber h = []
-- 		 | otherwise  = h : shortArgs tl

----------------------------------------------------------------------------------------------------

nest :: Int -> String -> String
nest n str = remlastNewline $ unlines $ 
             map (replicate n ' ' ++) $
             lines str
 where
   remlastNewline str =
     case reverse str of
       '\n':rest -> reverse rest
       _         -> str
