{-# LANGUAGE BangPatterns, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleContexts #-}
{-# LANGUAGE CPP, OverloadedStrings, TupleSections #-}
--------------------------------------------------------------------------------
-- NOTE: This is best when compiled with "ghc -threaded"
-- However, ideally for real benchmarking runs we WANT the waitForProcess below block the whole process.
-- However^2, currently [2012.05.03] when running without threads I get errors like this:
--   benchmark.run: bench_hive.log: openFile: resource busy (file is locked)

--------------------------------------------------------------------------------

{- | The Main module defining the HSBencher driver.
-}

module HSBencher.Internal.App
       (defaultMainModifyConfig,
        Flag(..), all_cli_options, fullUsageInfo)
       where 

----------------------------
-- Standard library imports
import Control.Concurrent
import qualified Control.Concurrent.Async as A
import Control.Exception (SomeException, try, catch)
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.List (intercalate, sortBy, intersperse, isInfixOf)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Version (versionBranch)
import Data.Word (Word64)
import Numeric (showFFloat)
import Prelude hiding (log)
import System.Console.GetOpt (getOpt', ArgOrder(Permute), OptDescr, usageInfo)
import System.Directory
import System.Environment (getArgs, getEnv, getProgName)
import System.Exit
import System.FilePath (splitFileName, (</>))
import System.Process (CmdSpec(..), readProcess)
import Text.Printf

----------------------------
-- Additional libraries:
import qualified System.IO.Streams as Strm
import qualified System.IO.Streams.Concurrent as Strm

#ifdef USE_HYDRAPRINT
import UI.HydraPrint (hydraPrint, HydraConf(..), DeleteWinWhen(..), defaultHydraConf, hydraPrintStatic)
import Scripting.Parallel.ThreadPool (parForM)
#endif

----------------------------
-- Self imports:

import HSBencher.Types
import HSBencher.Internal.Utils
import HSBencher.Internal.Logging
import HSBencher.Internal.Config
import HSBencher.Internal.MeasureProcess  (measureProcess,measureProcessDBG)
import Paths_hsbencher (version) -- Thanks, cabal!

----------------------------------------------------------------------------------------------------

hsbencherVersion :: String
hsbencherVersion = concat $ intersperse "." $ map show $ 
                   versionBranch version

-- | General usage information.
generalUsageStr :: String
generalUsageStr = unlines $
 [
   "   ",         
{-
   " Many of these options can redundantly be set either when the benchmark driver is run,",
   " or in the benchmark descriptions themselves.  E.g. --with-ghc is just for convenience.",
   "\n ENV VARS:",

-- No ENV vars currently! [2014.04.09]

   "   These environment variables control the behavior of the benchmark script:",
   " ",
   "   Command line arguments take precedence over environment variables, if both apply.",
   "   ",
-}
   " Note: This bench harness was built against hsbencher library version "++hsbencherVersion
 ]

  
--------------------------------------------------------------------------------
-- Compiling Benchmarks
--------------------------------------------------------------------------------

-- | Build a single benchmark in a single configuration.
compileOne :: (Int,Int) -> Benchmark DefaultParamMeaning -> [(DefaultParamMeaning,ParamSetting)] -> BenchM BuildResult
compileOne (iterNum,totalIters) Benchmark{target=testPath,cmdargs, overrideMethod} cconf = do
  cfg@Config{buildMethods, pathRegistry, doClean} <- ask

  let (_diroffset,testRoot) = splitFileName testPath
      flags = toCompileFlags cconf
      paths = toCmdPaths     cconf
      bldid = makeBuildID testPath flags
  log  "\n--------------------------------------------------------------------------------"
  log$ "  Compiling Config "++show iterNum++" of "++show totalIters++
       ": "++testRoot++" (args \""++unwords cmdargs++"\") confID "++ show bldid
  log  "--------------------------------------------------------------------------------\n"

  matches <- case overrideMethod of
    Nothing -> lift$
               filterM (fmap isJust . (`filePredCheck` testPath) . canBuild) buildMethods
    Just m -> return [m]
  when (null matches) $ do
       logT$ "ERROR, no build method matches path: "++testPath
       logT$ "  Tried methods: "++show(map methodName buildMethods)
       logT$ "  With file preds: "
       forM buildMethods $ \ meth ->
         logT$ "    "++ show (canBuild meth)
       lift exitFailure     
  logT$ printf "Found %d methods that can handle %s: %s" 
         (length matches) testPath (show$ map methodName matches)
  let BuildMethod{methodName,clean,compile} = head matches
  when (length matches > 1) $
    logT$ " WARNING: resolving ambiguity, picking method: "++methodName

  -- Add the static path information to the path information for this specific benchmark:
  let newpathR = (M.union (M.fromList paths) pathRegistry)
  
  when doClean $ clean newpathR bldid testPath

  -- This is a bit weird... we could recast ALL fields of the Config
  -- by specializing them to the benchmark particular compile
  -- configuration.  But right now we are doing that just for
  -- pathRegistry:
  let cfg2 = cfg{pathRegistry=newpathR}

  -- Prefer the benchmark-local path definitions:
  x <- compile cfg2 bldid flags testPath
  logT$ "Compile finished, result: "++ show x
  return x
  

--------------------------------------------------------------------------------
-- Running Benchmarks
--------------------------------------------------------------------------------

-- If the benchmark has already been compiled doCompile=False can be
-- used to skip straight to the execution.
--
-- runconfig contains both compile time and runtime parameters.  All
-- the params that affect this run.
runOne :: (Int,Int) -> BuildID -> BuildResult 
       -> Benchmark DefaultParamMeaning 
       -> [(DefaultParamMeaning,ParamSetting)] -> BenchM Bool
runOne (iterNum, totalIters) _bldid bldres
       Benchmark{target=testPath, cmdargs, progname, benchTimeOut}
       runconfig = do       

  log$ "\n--------------------------------------------------------------------------------"
  log$ "  Running Config "++show iterNum++" of "++show totalIters ++": "++testPath++" "++unwords cmdargs
--       "  threads "++show numthreads++" (Env="++show envVars++")"

  -- (1) Gather contextual information
  ----------------------------------------  
  -- Fullargs includes the runtime parameters as well as the original args:
  (args, fullargs, testRoot) <- runA_gatherContext testPath cmdargs runconfig

  -- (2) Now execute N trials:
  ----------------------------------------
  -- TODO (One option woud be dynamic feedback where if the first one
  -- takes a long time we don't bother doing more trials.)
  (retries,nruns) <- runB_runTrials fullargs benchTimeOut bldres runconfig

  -- (3) Produce output to the right places:
  ------------------------------------------
  runC_produceOutput (args,fullargs) (retries,nruns) testRoot progname runconfig


------------------------------------------------------------
runA_gatherContext :: FilePath -> [String] -> [(a, ParamSetting)] -> ReaderT Config IO ([String], [String], FilePath)
runA_gatherContext testPath cmdargs runconfig = do 
  Config{shortrun, argsBeforeFlags} <- ask
  let runParams = [ s | (_,RuntimeParam s) <- runconfig ]
      runArgs   = [ s | (_,RuntimeArg s) <- runconfig ]
      args0 = cmdargs ++ runArgs 
  let args = if shortrun then shortArgs args0 else args0
  let fullargs = if argsBeforeFlags 
                 then args ++ runParams
                 else runParams ++ args
      testRoot = fetchBaseName testPath
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
  return (args,fullargs,testRoot)


------------------------------------------------------------
runB_runTrials :: [String] -> Maybe Double -> BuildResult 
               -> [(DefaultParamMeaning, ParamSetting)] -> ReaderT Config IO (Int,[RunResult])
runB_runTrials fullargs benchTimeOut bldres runconfig = do 
    Config{ retryFailed, trials } <- ask 
    let retryBudget = fromMaybe 0 retryFailed
    trialLoop 1 trials retryBudget 0 []
 where   
  trialLoop :: Int -> Int -> Int -> Int -> [RunResult] -> ReaderT Config IO (Int,[RunResult])
  trialLoop ind trials retries retryAcc acc 
   | ind > trials = return (retryAcc, reverse acc)
   | otherwise = do 
    Config{ runTimeOut, shortrun, harvesters, systemCleaner } <- ask 
    log$ printf "  Running trial %d of %d" ind trials
    log "  ------------------------"
    case systemCleaner of 
      NoCleanup   -> return ()
      Cleanup act -> lift $ do 
                       printf "(Cleaning system with user-specified action to achieve an isolated run...)\n"
                       catch act $ \ (e::SomeException) -> 
                          printf $ "WARNING! user-specified cleanup action threw an exception:\n  "++show e++"\n"
    let envVars = toEnvVars  runconfig
    let affinity = getAffinity runconfig 
    
    let doMeasure1 cmddescr = do
          SubProcess {wait,process_out,process_err} <-
            lift$ measureProcess affinity harvesters cmddescr
          err2 <- lift$ Strm.map (B.append " [stderr] ") process_err
          both <- lift$ Strm.concurrentMerge [process_out, err2]
          mv   <- echoStream (not shortrun) both
          x    <- lift wait
          lift$ A.wait mv
          logT$ " Subprocess finished and echo thread done.\n"
          return x    

    -- I'm having problems currently [2014.07.04], where after about
    -- 50 benchmarks (* 3 trials), all runs fail but there is NO
    -- echo'd output. So here we try something simpler as a test.
    let doMeasure2 cmddescr = do
          (lines,result) <- lift$ measureProcessDBG affinity harvesters cmddescr 
          mapM_ (logT . B.unpack) lines 
          logT $ "Subprocess completed with "++show(length lines)++" of output."
          return result

        doMeasure = doMeasure2  -- TEMP / Toggle me back later.
        --doMeasure = doMeasure1  -- TEMP / Toggle me back later.

    this <- case bldres of
      StandAloneBinary binpath -> do
        -- NOTE: For now allowing rts args to include things like "+RTS -RTS", i.e. multiple tokens:
        let command = binpath++" "++unwords fullargs 
        logT$ " Executing command: " ++ command
        let timeout = if benchTimeOut == Nothing
                      then runTimeOut
                      else benchTimeOut
        case timeout of
          Just t  -> logT$ " Setting timeout: " ++ show t
          Nothing -> return ()
        doMeasure CommandDescr{ command=ShellCommand command, envVars, timeout, workingDir=Nothing, tolerateError=False }
      RunInPlace fn -> do
--        logT$ " Executing in-place benchmark run."
        let cmd = fn fullargs envVars
        logT$ " Generated in-place run command: "++show cmd
        doMeasure cmd

    if isError this 
     then if retries > 0 
          then do logT$ " Failed Trial!  Retrying config, repeating trial "++
                        show ind++", "++show (retries - 1)++" retries left."
                  trialLoop ind trials (retries - 1) (retryAcc + 1) acc
          else do logT$ " Failed Trial "++show ind++"!  Out of retries, aborting remaining trials."
                  return (retries, this:acc)
     else do -- When we advance, we reset the retry counter:
             Config{ retryFailed } <- ask 
             trialLoop (ind+1) trials (fromMaybe 0 retryFailed) retryAcc (this:acc)

getAffinity :: [(DefaultParamMeaning, ParamSetting)] -> Maybe (Int, CPUAffinity)
getAffinity cfg = case [ aff | (_, CPUSet aff) <- cfg ] of
                    []  -> Nothing
                    [x] -> Just (getNumThreads cfg, x)
                    ls  -> error$"hsbencher: got more than one CPUAffinity setting: "++show ls

getNumThreads :: [(DefaultParamMeaning, ParamSetting)] -> Int
getNumThreads = foldl (\ acc (x,_) ->
                           case x of
                             Threads n -> n
                             _         -> acc)
                   0 

------------------------------------------------------------
runC_produceOutput :: ([String], [String]) -> (Int,[RunResult]) -> String -> Maybe String 
                   -> [(DefaultParamMeaning, ParamSetting)] -> ReaderT Config IO Bool
runC_produceOutput (args,fullargs) (retries,nruns) testRoot progname runconfig = do
  let numthreads = getNumThreads runconfig 
      sched      = foldl (\ acc (x,_) ->
                           case x of
                             Variant s -> s
                             _         -> acc)
                   "none" runconfig

  let pads n s = take (max 1 (n - length s)) $ repeat ' '
      padl n x = pads n x ++ x 
      padr n x = x ++ pads n x
  let thename = case progname of
                  Just s  -> s
                  Nothing -> testRoot
  Config{ keepgoing } <- ask 
  let exitCheck = when (any isError nruns && not keepgoing) $ do 
                    log $ "\n Some runs were ERRORS; --keepgoing not used, so exiting now."
                    liftIO exitFailure
                    
  -- FIXME: this old output format can be factored out into a plugin or discarded:
  (_t1,_t2,_t3,_p1,_p2,_p3) <-
    if all isError nruns then do
      log $ "\n >>> MIN/MEDIAN/MAX (TIME,PROD) -- got only ERRORS: " ++show nruns
      logOn [ResultsFile]$ 
        printf "# %s %s %s %s %s" (padr 35 thename) (padr 20$ intercalate "_" fullargs)
                                  (padr 8$ sched) (padr 3$ show numthreads) (" ALL_ERRORS"::String)
      exitCheck
      return ("","","","","","")
    else do
      exitCheck
      let goodruns = filter (not . isError) nruns
      -- Extract the min, median, and max:
          sorted = sortBy (\ a b -> compare (gettime a) (gettime b)) goodruns
          minR = head sorted
          maxR = last sorted
          medianR = sorted !! (length sorted `quot` 2)

      let ts@[t1,t2,t3]    = map (\x -> showFFloat Nothing x "")
                             [gettime minR, gettime medianR, gettime maxR]
          prods@[p1,p2,p3] = map mshow [getprod minR, getprod medianR, getprod maxR]
          mshow Nothing  = "0"
          mshow (Just x) = showFFloat (Just 2) x "" 

          -- These are really (time,prod) tuples, but a flat list of
          -- scalars is simpler and readable by gnuplot:
          formatted = (padl 15$ unwords $ ts)
                      ++"   "++ unwords prods -- prods may be empty!

      log $ "\n >>> MIN/MEDIAN/MAX (TIME,PROD) " ++ formatted

      logOn [ResultsFile]$ 
        printf "%s %s %s %s %s" (padr 35 thename) (padr 20$ intercalate "_" fullargs)
                                (padr 8$ sched) (padr 3$ show numthreads) formatted

      -- These should be either all Nothing or all Just:
      let jittimes0 = map getjittime goodruns
          misses = length (filter (==Nothing) jittimes0)
      jittimes <- if misses == length goodruns
                  then return ""
                  else if misses == 0
                       then return $ unwords (map (show . fromJust) jittimes0)
                       else do log $ "WARNING: got JITTIME for some runs: "++show jittimes0
                               log "  Zeroing those that did not report."
                               return $ unwords (map (show . fromMaybe 0) jittimes0)

      let affinity = getAffinity runconfig 

      Config{ trials } <- ask 
      let result =
            emptyBenchmarkResult
            { _PROGNAME = case progname of
                           Just s  -> s
                           Nothing -> testRoot
            , _VARIANT  = sched
            , _ARGS     = args
            , _THREADS  = numthreads
            , _MINTIME    =  gettime minR
            , _MEDIANTIME =  gettime medianR
            , _MAXTIME    =  gettime maxR
            , _MINTIME_PRODUCTIVITY    = getprod minR
            , _MEDIANTIME_PRODUCTIVITY = getprod medianR
            , _MEDIANTIME_ALLOCRATE    = getallocrate medianR
            , _MEDIANTIME_MEMFOOTPRINT = getmemfootprint medianR
            , _MAXTIME_PRODUCTIVITY    = getprod maxR
            , _RUNTIME_FLAGS =  unwords [ s | (_,RuntimeParam s) <- runconfig ]
            , _COMPILE_FLAGS =  show (toCompileFlags runconfig)
            , _ALLTIMES      =  unwords$ map (show . gettime)    goodruns
            , _ALLJITTIMES   =  jittimes
            , _TRIALS        =  trials
            , _TOPOLOGY      =  show affinity
            , _RETRIES       =  retries
                                -- Should the user specify how the
                                -- results over many goodruns are reduced ?
                                -- I think so. 
            , _CUSTOM        = custom (head goodruns) -- experimenting 
            }
      conf <- ask
      result' <- liftIO$ augmentResultWithConfig conf result

      -- Upload results to plugin backends:
      conf2@Config{ plugIns } <- ask 
      forM_ plugIns $ \ (SomePlugin p) -> do 

        --JS: May 21 2014, added try and case on result. 
        result3 <- liftIO$ try (plugUploadRow p conf2 result') :: ReaderT Config IO (Either SomeException ()) 
        case result3 of
          Left err -> logT$("plugUploadRow:Failed, error: \n"++
                            "------------------begin-error----------------------\n"++
                            show err ++
                            "\n-------------------end-error-----------------------\n"
                            )
          Right () -> return ()
        return ()

      return (t1,t2,t3,p1,p2,p3)

  -- If -keepgoing is set, we consider only errors on all runs to
  -- invalidate the whole benchmark job:
  return (not (all isError nruns))





--------------------------------------------------------------------------------


-- | Write the results header out stdout and to disk.
printBenchrunHeader :: BenchM ()
printBenchrunHeader = do
  Config{trials, maxthreads, pathRegistry, defTopology,
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
             , e$ "# Default topology: "++defTopology
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


-- | TODO: Eventually this will make sense when all config can be read from the environment, args, files.
defaultMain :: IO ()
defaultMain = do
  --      benchF = get "BENCHLIST" "benchlist.txt"
--  putStrLn$ hsbencher_tag ++ " Reading benchmark list from file: "
  error "FINISHME: defaultMain requires reading benchmark list from a file.  Implement it!"
--  defaultMainWithBenchmarks undefined

-- | In this version, user provides a list of benchmarks to run, explicitly.
defaultMainWithBenchmarks :: [Benchmark DefaultParamMeaning] -> IO ()
defaultMainWithBenchmarks benches = do
  defaultMainModifyConfig (\ conf -> conf{ benchlist=benches })

-- | Multiple lines of usage info help docs.
fullUsageInfo :: String
fullUsageInfo = 
    "\nUSAGE: naked command line arguments are patterns that select the benchmarks to run.\n"++
    (concat (map (uncurry usageInfo) all_cli_options)) ++
    generalUsageStr

    
-- | Remove a plugin from the configuration based on its plugName
removePlugin :: Plugin p => p -> Config -> Config 
removePlugin p cfg = 
  cfg { plugIns = filter byNom  (plugIns cfg)}
  where
    byNom (SomePlugin p1) =  plugName p1 /= plugName p


--------------------------------------------------------------------------------

doShowHelp :: [SomePlugin] -> IO ()
doShowHelp allplugs = do
    putStrLn$ "\nUSAGE: [set ENV VARS] "++my_name++" [CMDLN OPTS]"
    putStrLn$ "\nNote: \"CMDLN OPTS\" includes patterns that select which benchmarks"
    putStrLn$ "     to run, based on name."
    mapM putStr (map (uncurry usageInfo) all_cli_options)
    putStrLn ""
    putStrLn $ show (length allplugs) ++ " plugins enabled: "++ 
               show [ plugName p | SomePlugin p <- allplugs ]
    putStrLn ""
    forM_ allplugs $ \ (SomePlugin p) -> do  
      putStrLn $ "["++ plugName p++"] "++ ((uncurry usageInfo) (plugCmdOpts p))
    putStrLn$ generalUsageStr

-- TODO/FIXME: Break up the giant function below.  Also move to using
-- a StateT to store the Config, and reduce the verbosity of the
-- output when "Harvesting environment data".

-- | An even more flexible version allows the user to install a hook which modifies
-- the configuration just before bencharking begins.  All trawling of the execution
-- environment (command line args, environment variables) happens BEFORE the user
-- sees the configuration.
--
-- This function doesn't take a benchmark list separately, because that simply
-- corresponds to the 'benchlist' field of the output 'Config'.
defaultMainModifyConfig :: (Config -> Config) -> IO ()
defaultMainModifyConfig modConfig = do    
  id       <- myThreadId
  writeIORef main_threadid id
  my_name  <- getProgName
  cli_args <- getArgs

  let (options,plainargs,_unrec,errs) = getOpt' Permute (concat$ map snd all_cli_options) cli_args

  -- This ugly method avoids needing an Eq instance:
  let recomp       =      null [ () | NoRecomp <- options]
      showHelp     = not$ null [ () | ShowHelp <- options]
      gotVersion   = not$ null [ () | ShowVersion <- options]
      showBenchs   = not$ null [ () | ShowBenchmarks <- options]
      cabalAllowed = not$ null [ () | NoCabal  <- options]
      parBench     = not$ null [ () | ParBench <- options]
      disabled     = [ s | DisablePlug s <- options ]

  when gotVersion  $ do
    putStrLn$ "hsbencher version "++ hsbencherVersion
      -- (unwords$ versionTags version)
    exitSuccess 

  ------------------------------------------------------------
  putStrLn$ "\n"++hsbencher_tag++"Harvesting environment data to build Config."
  conf0 <- getConfig options []
  -- The list of benchmarks can optionally be narrowed to match any of the given patterns.
  let conf1 = modConfig conf0
  -- The phasing here is rather funny.  We need to get the initial config to know
  -- WHICH plugins are active.  And then their individual per-plugin configs need to
  -- be computed and added to the global config.
  let plugnames = [ plugName p | SomePlugin p <- plugIns conf1 ]

  let plugs = [ if (or [ isInfixOf d (plugName p)| d <- disabled ])
                then Right (plugName p, SomePlugin p) -- Disabled
                else Left  (plugName p, SomePlugin p) -- Enabled
              | SomePlugin p <- plugIns conf1 ]
  let offplugs = [ n  | Right (n, _)  <- plugs ]
      allplugs = [ sp | Left  (_, sp) <- plugs ]

  unless (null offplugs) $ 
    putStrLn $ hsbencher_tag ++ " DISABLED plugins that were compiled/linked in: "++unwords offplugs

  ------------------------------------------------------------
  let fullBenchList = 
       case conf1 of 
        Config{benchlist=ls} -> 
          (unlines  [ (maybe "" (++" = ") progname) ++
                      (target ++ (unwords cmdargs))
                    | Benchmark{progname, cmdargs,target} <- ls])
  when showBenchs $ do putStrLn ("All benchmarks handled by this script:\n"++fullBenchList)
                       exitSuccess
  unless (null errs) $ do
    putStrLn$ "Errors parsing command line options:"
    mapM_ (putStr . ("   "++)) errs       
    doShowHelp allplugs
    exitFailure
  when showHelp   $ do doShowHelp    allplugs; exitSuccess

  ------------------------------------------------------------
  -- Fully populate the per-plugin configurations, folding in command line args:
  -- 
  -- Hmm, not really a strong reason to *combine* the options lists, rather we do
  -- them one at a time:
  let pconfs = [ (plugName p, SomePluginConf p pconf)
               | (SomePlugin p) <- (plugIns conf1)
               , let (_pusage,popts) = plugCmdOpts p
               , let (o2,_,_,_) = getOpt' Permute popts cli_args 
               , let pconf = foldFlags p o2 (getMyConf p conf1)
               ]

  let conf2 = conf1 { plugInConfs = M.fromList pconfs }
  -- Combine all plugins command line options, and reparse the command line.

  putStrLn$ hsbencher_tag++(show$ length allplugs)++" plugins configured ("++ 
            concat (intersperse ", " [ plugName p | SomePlugin p <- allplugs ])
            ++"), now initializing them."

  -- TODO/FIXME: CATCH ERRORS... should remove the plugin from the list if it errors on init.
  -- JS attempted fix
  conf3 <- foldM (\ cfg (SomePlugin p) ->
                        do result <- try (plugInitialize p cfg) :: IO (Either SomeException Config) 
                           case result of
                             Left err -> do
                               putStrLn (hsbencher_tag++"Plugin Init FAILED!  Error:\n"++show err)
                               return $ removePlugin p cfg
                               -- cannot log here, only "chatter". 
                             Right c -> return c 
                 ) conf2 allplugs
  putStrLn$ hsbencher_tag++" plugin init complete."

  -------------------------------------------------------------------
  -- Next prune the list of benchmarks to those selected by the user:
  let cutlist = case plainargs of
                 [] -> benchlist conf3
                 patterns -> filter (\ Benchmark{target,cmdargs,progname} ->
                                      any (\pat ->
                                            isInfixOf pat target ||
                                            isInfixOf pat (fromMaybe "" progname) ||
                                            any (isInfixOf pat) cmdargs
                                          )
                                          patterns)
                                    (benchlist conf3)
  let conf4@Config{extraParams} = conf3{benchlist=cutlist}

  -- Finally, put the extra Params right into the benchmark config
  -- spaces at the last minute:
  let conf5@Config{benchlist} = L.foldr andAddParam conf4 extraParams
    
  ------------------------------------------------------------
  rootDir <- getCurrentDirectory  
  runReaderT 
    (do
        unless (null plainargs) $ do
          let len = (length cutlist)
          logT$"There were "++show len++" benchmarks matching patterns: "++show plainargs
          when (len == 0) $ do 
            error$ "Expected at least one pattern to match!.  All benchmarks: \n"++
                   fullBenchList
        
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
        printloop M.empty benchlist
        log$ "--------------------------------------------------------------------------------"

        if parBench then do
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

          -- Config{extraParams} <- ask
          -- let addExtras :: [(DefaultParamMeaning,ParamSetting)] -> [(DefaultParamMeaning,ParamSetting)]
          --     addExtras ls = (map (NoMeaning,) extraParams) ++ ls
          -- let allruns :: [[[(DefaultParamMeaning,ParamSetting)]]]
          --     allruns = map (map addExtras . enumerateBenchSpace . configs) benchlist
          let allruns :: [[[(DefaultParamMeaning,ParamSetting)]]]
              allruns = map (enumerateBenchSpace . configs) benchlist
              allrunsLens = map length allruns
              totalruns = sum allrunsLens
          let 
              -- Here we lazily compile benchmarks as they become required by run configurations.
              runloop :: Int 
                      -> M.Map BuildID (Int, Maybe BuildResult)
                      -> M.Map FilePath BuildID -- (S.Set ParamSetting)
                      -> [(Benchmark DefaultParamMeaning, [(DefaultParamMeaning,ParamSetting)])]
                      -> Bool -> BenchM Bool
              runloop _ _ _ [] b = return b
              runloop !iter !board !lastConfigured (nextrun:rest) allpassed = do
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
                    b <- runOne (iter,totalruns) bid res bench params
                    runloop (iter+1) board' lastC' rest (allpassed && b)

                  Just (ccnum, Just bldres) -> 
                    let proceed = do b <- runOne (iter,totalruns) bid bldres bench params
                                     runloop (iter+1) board lastConfigured rest (allpassed && b)
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
                           else runloop iter (M.insert bid (ccnum,Nothing) board) 
                                        lastConfigured (nextrun:rest) allpassed

              -- Keeps track of what's compiled.
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

          log$ " Begining execution of "++show totalcomps++" compiles and "++show totalruns++" run configs..."

          unless recomp $ logT$ "Recompilation disabled, assuming standalone binaries are in the expected places!"
          let startBoard = initBoard 1 zippedruns M.empty
          Config{skipTo, runOnly} <- ask
          (ix,runs') <- case skipTo of 
                          Nothing -> return (1,zippedruns)
                          Just ix -> do logT$" !!! WARNING: SKIPPING AHEAD in configuration space; jumping to: "++show ix
                                        return (ix, drop (ix-1) zippedruns)
          runs'' <- case runOnly of 
                      Nothing  -> return runs'
                      Just num -> do logT$" !!! WARNING: TRUNCATING config space to only run "++show num++" configs."
                                     return (take num runs')
          win <- runloop ix startBoard M.empty runs'' True                                
  	  unless win $ do 
             log$ "\n--------------------------------------------------------------------------------"
             log "  Finished benchmarks, but some errored out, marking this job as a failure."
             log$ "--------------------------------------------------------------------------------"
             liftIO$ exitFailure
          return ()
{-
        do Config{logOut, resultsOut, stdOut} <- ask
           liftIO$ Strm.write Nothing logOut 
           liftIO$ Strm.write Nothing resultsOut 
-}
        log$ "\n--------------------------------------------------------------------------------"
        log "  Finished with all benchmark configurations.  Success."
        log$ "--------------------------------------------------------------------------------"
	liftIO$ exitSuccess
    )
    conf5


-- Several different options for how to display output in parallel:
catParallelOutput :: [Strm.InputStream B.ByteString] -> Strm.OutputStream B.ByteString -> IO ()
catParallelOutput strms stdOut = do 
 case 4 of
#ifdef USE_HYDRAPRINT   
   -- First option is to create N window panes immediately.
   1 -> do
           hydraPrintStatic defaultHydraConf (zip (map show [1..]) strms)
   2 -> do
           srcs <- Strm.fromList (zip (map show [1..]) strms)
           hydraPrint defaultHydraConf{deleteWhen=Never} srcs
#endif
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

didComplete :: RunResult -> Bool
didComplete RunCompleted{} = True
didComplete _              = False

isError :: RunResult -> Bool
isError ExitError{} = True
isError _           = False

getprod :: RunResult -> Maybe Double
getprod RunCompleted{productivity} = productivity
getprod RunTimeOut{}               = Nothing
getprod x                          = error$"Cannot get productivity from: "++show x

getallocrate :: RunResult -> Maybe Word64
getallocrate RunCompleted{allocRate} = allocRate
getallocrate _                       = Nothing

getmemfootprint :: RunResult -> Maybe Word64
getmemfootprint RunCompleted{memFootprint} = memFootprint
getmemfootprint _                          = Nothing

gettime :: RunResult -> Double
gettime RunCompleted{realtime} = realtime
gettime RunTimeOut{}           = posInf
gettime x                      = error$"Cannot get realtime from: "++show x

getjittime :: RunResult -> Maybe Double
getjittime RunCompleted{jittime}  = jittime
getjittime _                      = Nothing

posInf :: Double
posInf = 1/0


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
