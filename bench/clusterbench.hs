{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, NamedFieldPuns, CPP #-}

-- Benchmark different configurations over a cluster of machines.
--
-- This includes the ability to search for unused machines and run benchmarks there.
-- 
-- This script is a layer on top of benchmark.hs and file_away.hs.
-- It may need to be compiled due to problems with HSH and ghci [2012.01.14].


--------------------------------------------------------------------------------
-- TODO: 

--  * Add robustness to disconnection (finish strongSSH)
--  * Check "who" repeatedly rather than just at the outset.

--  * Record all output from remote commands in a local per-worker logfile 

--  * Record a list of failed configurations and print them at the end.

--------------------------------------------------------------------------------

import HSH
import HSH.ShellEquivs
import Control.Monad 
import Control.Concurrent
import Control.Exception (catch, SomeException)
import Data.Maybe
import Data.Char (isSpace, isAlphaNum, isNumber)
import Data.List
import Data.IORef
import Data.Word
import Data.Typeable (Typeable, typeOf)
--import qualified Data.Array.IO as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Set as S
import qualified Data.Map as M
import System.Console.GetOpt
import System.Environment (getEnv, getArgs)
import System.Exit
import System.Process   (readProcessWithExitCode)
import System.IO        (hPutStrLn, stderr, stdout)
import System.Random    (randomIO)
import System.Directory (doesDirectoryExist, getCurrentDirectory, canonicalizePath,
			 createDirectoryIfMissing, doesFileExist, removeDirectory)
import System.FilePath (dropTrailingPathSeparator, makeRelative, 
			addTrailingPathSeparator, takeDirectory, (</>), (<.>))
import Text.Regex 
import Text.PrettyPrint.HughesPJClass (Pretty, pPrint, text)
import Text.PrettyPrint.HughesPJ (nest)
--------------------------------------------------------------------------------
-- Type definitions:


-- | Command line Flags.
data Flag = Git
          | MachineList [String]
          | ListIdle 
          -- Inclusive lower and upper bounds on which configurations to execute:
--          | ConfigRangeLowerBound Int
--          | ConfigRangeUpperBound Int

          -- Redo the run in directory run_N, replenishing whatever is not complete.
          | RedoRun Int 
 deriving (Show, Eq, Read)


-- | Benchmark configuration spaces.
data BenchmarkSetting =  
   -- Vary parameter:
   Vary ParamType [String]
   -- Set parameter across all runs:
 | Set ParamType String
 | Command String 


 -- The semantics of GitRepo are that it refers to the current head of a specific branch.
 -- | GitRepo { url :: String, branch :: String, dir :: String }
 | GitRepo String String String  -- Url, Branch, WorkingDir
 deriving (Show, Eq, Read, Typeable)

instance Pretty BenchmarkSetting where
  pPrint x = text $ show x 


-- | Different types of parameters that may be set or varied.
data ParamType = RTS     String   -- e.g. "-A"
	       | Compile String   -- e.g. "--ddump-simpl"
               | EnvVar  String
 deriving (Show, Eq, Read)


-- | The full configuration (list of parameter settings) for a single run.
data OneRunConfig = OneRunConfig { failcount :: Int, 
				   binds ::[(ParamType,String)] }
  deriving (Show,Eq)

-- How many times do we try a single config before giving up on it.
max_config_tries = 3


-- | An abstract executor for remote commands.
data Executor = Executor 
  { host :: String, 
    -- Remote execution of a series of shell commands:
    -- Returns shell output if successfull.
    runcmd :: [String] -> IO (Maybe B.ByteString),
    lockRemote   :: IO Bool,
    unlockRemote :: IO ()
  }

-- This contains a specific hash to checkout.
data FullGitLoc = FullGitLoc { url :: String, hash  :: String, dir :: String }
  deriving (Show,Eq)

-- The worker nodes do their work in a temporary location, but can
-- direct their logs into a central (typically NFS) location.
tmp_location = "/tmp/"

--------------------------------------------------------------------------------

-- <boilerplate>  Should be derivable:
isMachineList (MachineList _) = True 
isMachineList _               = False
unMachineList (MachineList ls) = ls
isEnvVar (EnvVar _) = True
isEnvVar _ = False
isRTS    (RTS _) = True
isRTS    _       = False
isCompile (Compile _) = True
isCompile          _  = False
isVary (Vary _ _) = True
isVary          _ = False
isSet  (Set _ _)  = True
isSet           _ = False
isCommand (Command _) = True
isCommand           _ = False
isGitRepo (GitRepo _ _ _) = True
isGitRepo           _ = False
isRedoRun (RedoRun _) = True
isRedoRun _           = False

getCommand :: [BenchmarkSetting] -> String
getCommand settings = 
  case filter isCommand settings of 
   []  -> error "No Command form in benchmark description."
   [Command c] -> c
   _   -> error "More than one Command form in benchmark description."

-- </boilerplate>

-- | Command line options.
cli_options :: [OptDescr Flag]
cli_options = 
     [ Option ['g'] ["git"] (NoArg Git) 
          "Assume the current directory is part of a clonable git working copy."
     , Option ['m'] ["machines"] 
          (ReqArg (MachineList . words) "HOSTS")
          "list of machines to use for running benchmarks"

     -- Not implemented yet.  This is only useful if you KNOW that you
     -- haven't missed anything up to N:
     -- , Option [] ["skipto"] 
     --      (ReqArg (ConfigRangeLowerBound . read) "NUM")
     --      "skip ahead to start at NUM configuration"

     , Option [] ["retry"]
          (ReqArg (RedoRun . read) "NUM")
          "redo run_NUM replinishing configurations that did not complete successfully"

     , Option ['l'] ["listidle"] 
          (NoArg ListIdle)
          "don't run anything, just list idle machines."
--      , Option ['f'] ["config"] 
--           (ReqArg ConfigFile "FILE")
--           "read benchmark configuration from file"
     ]

------------------------------------------------------------
-- Configuration helpers

readConfig :: String -> [BenchmarkSetting]
readConfig s = 
  myread $ trim $ unlines $
  -- We allow comments in the file.
  -- (Scheme readers handle this by default.)
  filter (not . isCommentLine) $ 
  lines s 

isCommentLine :: String -> Bool
isCommentLine = isPrefixOf "--" . trim 
 -- Cannot get this to work:
--  (not . isJust . (matchRegex$ mkRegexWithOpts "^\\s*\\-\\-" True True)) $  


countConfigs :: [BenchmarkSetting] -> Int
countConfigs settings = 
  foldl (*) 1 $ 
  map (\ (Vary _ ls) -> length ls) $
  filter isVary $ 
  settings

-- | List out all combinations explicitly.
listConfigs :: [BenchmarkSetting] -> [OneRunConfig]
listConfigs settings = map (OneRunConfig 0) $
		       loop constant varies  
 where 
  constant = map (\ (Set x y) -> (x,y)) $ 
             filter isSet settings
  varies = filter isVary settings
  loop consts [] = [consts]
  loop consts (Vary param vals : varies) =
    concatMap (\ val -> loop ((param,val) : consts) varies ) 
              vals 
    
    

--------------------------------------------------------------------------------
-- Misc Helpers:

newtype TimeOut = Seconds Double

-- | Fork a list of computations with a timeout.  Return a lazy list
--   of the results.
forkCmds :: TimeOut -> [IO a] -> IO [a]
forkCmds (Seconds s) comps  = do
  chan <- newChan 
  let wrap = (>>= (writeChan chan . Just))
  ids <- mapM (forkWithExceptions forkIO "forked child command" . wrap) comps
  -- And a timeout thread:
  forkWithExceptions forkIO "timeout thread" $ 
     do threadDelay (floor$ s * 1000 * 1000)
        mapM_ killThread ids
        writeChan chan Nothing
  ls <- getChanContents chan 
  return (map fromJust $ 
	  takeWhile isJust $ 
          -- Make sure that we don't wait for the timeout if all respond:
	  -- There can no more than one response per computation:
	  take (length comps) ls)

atomicModifyIORef_ ref fn = atomicModifyIORef ref (\x -> (fn x, ()))


machineIdle :: String -> IO Bool
machineIdle host = do 
  localUser <- getEnv "USER"
  let 
      remoteUser = parseUser host
      user = case remoteUser of 
                Nothing -> localUser
		Just s  -> s 
#if 0
  -- I suspect this is the culprit that is creating zombie ssh
  -- processes... need to debug these HSH issues if it is going to be
  -- usable:
  who :: String <- run $ mkSSHCmd host ["who"] -|- grepV user
  return (null who)
#else
--  who :: String <- run $ mkSSHCmd host ["who"] -|- grepV user
  res <- strongSSH host ["who"]
  case res of 
    -- We don't stop execution over a polling ssh failing:
    Nothing -> return False
    Just output -> 
       let lns = map B.unpack $ B.lines output
           pruned = filter (not . isInfixOf user) $ 
                    filter (not . null) lns
       in return (null pruned)
#endif



-- Exceptions that walk up the fork tree of threads:
forkWithExceptions :: (IO () -> IO ThreadId) -> String -> IO () -> IO ThreadId
forkWithExceptions forkit descr action = do 
   parent <- myThreadId
   forkit $ 
      Control.Exception.catch action
	 (\ e -> do
	  B.hPutStrLn stderr $ B.pack $ "Exception inside child thread "++show descr++": "++show e
	  throwTo parent (e::SomeException)
	 )

------------------------------------------------------------
-- Directory and Path helpers

-- Get the current directory as a path of the form "~/foo" that will
-- work on another machine when expanded by the shell.
getPortableWD :: IO String
getPortableWD = do
  logical <- runSL "pwd -L"    -- getCurrentDirectory won't cut it.
  b <- isHomePath logical
  unless b$ error$ "getPortableWD: not a path under the home directory: "++logical
  makeHomePathPortable logical

makeHomePathPortable :: String -> IO String
makeHomePathPortable str = do 
  home    <- getEnv "HOME"
  let path = subRegex (mkRegex$ addTrailingPathSeparator home) str "~/"
  return (dropTrailingPathSeparator path)

-- And the reverse, get rid of ~/:
dePortablize :: String -> IO String
dePortablize ('~':'/':tl) = do 
  home <- getEnv "HOME" 
  return (home </> tl)
dePortablize path = return path

-- | Return a portable "~/foo" path describing the location of the git
--   working copy root containing the current working directory.  This
--   assumes that there is a parent directory containing a ".git"
--
--   It returns both the absolute location of the gitroot and the
--   relative offset inside it to reach the current dir.
findGitRoot :: IO FullGitLoc
findGitRoot = 
   do start <- runSL "pwd -L" 
      root  <- loop start
      canonA <- canonicalizePath root
      canonB <- canonicalizePath start
      let offset = makeRelative canonA canonB
      portroot <- makeHomePathPortable root

      -- Obscure way to get the current branch:
      branch <- runSL "git rev-parse --abbrev-ref HEAD"
      return (FullGitLoc portroot branch offset)
 where 
  loop path = do 
    b0 <- isHomePath path 
    unless b0 $ do
       putStrLn$ "ERROR, findGitRoot: descended out of the users home directory to: "++ path
       exitFailure 
    b <- doesDirectoryExist$ path </> ".git"
    if b then return path
         else loop (takeDirectory path)

-- | Is this a path under the home directory?
isHomePath :: String -> IO Bool
isHomePath str = do 
  home    <- getEnv "HOME"
  case matchRegex (mkRegex home) str of 
    Nothing -> return False
    Just _  -> return True

-- | Return Just "user" given "user@host.com".
parseUser :: String -> Maybe String
parseUser str = 
  case matchRegexAll (mkRegex ".*@") str of
     Nothing          -> Nothing
     Just (_,hit,_,_) -> Just (reverse$ tail$ reverse hit)



-- | Remove whitespace from both ends of a string:
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- | 'read' with much much better error messages.
myread :: (Read a, Typeable a) => String -> a
myread str = result
  where 
  typ = show (typeOf result)
  result = 
    case reads str of 
      [] -> error$ "read: Could not read as type "++typ++": "++
            "\n================================================================================\n" ++ 
	     str ++
            "\n================================================================================\n"
      (x,""):_   -> x
      (_,ext):_  -> error$ "read: valid parse as type "++typ++"but extra characters at the end: "++ ext

--------------------------------------------------------------------------------
-- idleExecutors: The heart of remote execution.

-- | An infinite stream of idle machines ready to do work.
idleExecutors :: [String] -> IO (MVar Executor)
-- Note: this is not a demand-driven stream.  It is an actor that
-- PUSHES a stream of idle workers. However the stream is implemented
-- as an MVar so it has a buffer size of one.  Thus this will try to
-- keep one step ahead of the consumer asking for executors.
idleExecutors hosts = do 
  -- The set of hosts that are actively running OUR jobs:
  activehosts <- newIORef S.empty

  let executors = M.fromList $ zip hosts (map makeone hosts)
      makeone h = 
        Executor { host   = h
		 , runcmd = runner activehosts h
		 , lockRemote   = locker   activehosts h
		 , unlockRemote = unlocker activehosts h
		 }

  strm <- newEmptyMVar
  let loop = do busies <- readIORef activehosts
                let remaining = filter (not . (`S.member` busies)) hosts
		if null remaining then do
		   putStrLn$ "  - Not polling... all nodes are busy, waiting 10 seconds..."
                 else do
		   putStrLn$ "  - Polling for idleness: "++unwords remaining
		   ls <- pollIdles remaining
		   let execs = map (executors M.!) ls
		   forM_ execs $ putMVar strm 
		threadDelay (10 * 1000 * 1000)
		loop

  forkWithExceptions forkIO "idleExecutors loop" $ loop 
  return strm
 where 
  locker activehosts host = do 
    -- When we are locked to run commands we add ourselves to the active/busy set:
--    atomicModifyIORef_ activehosts (S.insert host)
    b <- atomicModifyIORef activehosts (\set -> if S.member host set
					        then (set, False)
					        else (S.insert host set, True))
    when b $ putStrLn$ "  + LOCKED host for execution: "++host
    -- Right now we only track remote-machine activity CENTRALLY (on the master).
    -- TODO -- should also create a /tmp/ file on the remote to signify locking.
    -- This would enable us to run multiple clusterbench jobs on the same resources.
    return b

  unlocker activehosts host = do 
    putStrLn$ "  + UNLOCKING host: "++host
    -- Once we're all done with the commands, we can join the next round of polling:
    atomicModifyIORef_ activehosts (S.delete host)

  runner activehosts host cmds = do
    putStrLn$ "  + Executor on host "++host++" invoking commands: "++show cmds
    strongSSH host cmds

      
pollIdles :: [String] -> IO [String]
pollIdles []    =  return [] 
pollIdles hosts =  fmap catMaybes $ 
  	             forkCmds (Seconds 10) $  
		     map (\s -> do idle <- machineIdle s
			           return (if idle then Just s else Nothing)
			 ) hosts

--------------------------------------------------------------------------------

-- | SSH to start a job, but don't give up on disconnection.
--   Returns Nothing in the case of an unrecoverable error.
--
-- TODO: Finish this.  It should do something like the following to
-- record the PID:
--    ssh -n host '(sleep 100 & echo asynchpid $!) > /tmp/foo'
-- Then, after disconnection it should poll to see if the job is
-- really done, for example with "ps h 1234" or kill -0.
-- 
strongSSH :: String -> [String] -> IO (Maybe B.ByteString)
strongSSH host cmds = do
#if 0 
-- HAVING PROBLEMS:
  (output, checkResults) <- run (mkSSHCmd host cmds)
  (descr,code) <- checkResults
  case code of
    ExitSuccess -> return (Just output)
    ExitFailure cd -> do putStrLn$ "SSH command returned error code "++show cd++" "++descr
			 return Nothing
#else 
-- Having failure-to-exit problems with the above.  Trying System.Process directly:
   (code,out,err) <- readProcessWithExitCode "ssh" [host, concat (intersperse " && " cmds)] ""
   -- TODO: Dropping down to createProcess would allow using ByteStrings:
   case code of 
     -- When it succeeds we echo its errors anyway so that we can see errors that arise:
     ExitSuccess -> do forkWithExceptions forkIO "asynchronous print" $ putStr err  
		       return (Just$ B.pack out)
     ExitFailure _ -> do hPutStrLn stdout "------------------------------------------------------------"
                         hPutStrLn stdout " !!! strongSSH: Remote Command FAILURE: "
                         hPutStrLn stdout "------------------------------------------------------------"
                         hPutStrLn stdout out
			 hPutStrLn stderr err
                         hPutStrLn stdout "------------------------------------------------------------"
                         return Nothing
#endif


#if 0
-- | Construct a command string that will ssh into another machine and
--   run a series of commands.
mkSSHCmd :: String -> [String] -> String
  -- With no arguments it just makes sure that we can login:
mkSSHCmd host []    = "ssh "++host++" ' ' "
mkSSHCmd host cmds  = "ssh "++host++" '"++ concat (intersperse " && " cmds) ++ "'"
#endif


--------------------------------------------------------------------------------

-- | Job management.  Consume a stream of Idle Executors to schedule
--   jobs remotely.
launchConfigs :: Int -> [BenchmarkSetting] -> MVar Executor -> FullGitLoc -> String -> IO ()
launchConfigs run_N settings idles 
	      gitloc@(FullGitLoc gitroot branch gitoffset) 
	      startpath = do
  -- Create our big list of configs.  This enumerates our total work to do:
  let allconfs = listConfigs settings
  -- If a configuration run fails, it fails on another thread so we
  -- need a mutable structure to keep track of them.
  state <- newIORef (allconfs, M.empty)
  -- ^^^ This atomically modified state tracks the confs and outstanding/working
  -- threads (each of which is modeled by an mvar that will be written
  -- at completion.).

  -- The .log destination directory.  Preexisting or fresh:
  logd  <- makeLogDir run_N
  createDirectoryIfMissing True logd

  -- We use the same "run_N" structure inside the temp directory.
  -- This is the location to do a git clone and do the work:
  let tmpd = tmp_location </> basename logd

  -- The main job-scheduling loop:
  let finalcommand = getCommand settings
      loop = do
         peek <- readIORef state 
         case peek of 
           -- If there are no configurations left and no outstanding threads:
     	   ([],m) | M.null m -> putStrLn$ "Done with all configurations.  All jobs finished." 
           -- If there are no configurations BUT threads are still working:
     	   ([],m) -> do -- Here we atomically discharge the obligation to wait for outstanding workers.
		        mvs <- atomicModifyIORef state 
			        (\ (confs,mp) -> 
				  case confs of 
				   []  -> (([], M.empty), M.elems mp)
                                   -- If confs reappeared someone failed, forget it and loop:
				   _   -> ((confs, mp), []))
			putStrLn$ "  - launchConfigs: All launched, waiting for "++
				  show (length mvs)++" outstanding jobs to complete or fail."
			mapM_ readMVar mvs
			loop
           (confs,_)  -> do 

            putStrLn$ " * "++ show (length confs) ++" configurations left to process, waiting for next executor..."
	    -- We do the blocking operation INSIDE 'loop' to hold it back:
	    exec @ (Executor{host,runcmd,lockRemote,unlockRemote}) <- takeMVar idles  -- BLOCKING!

            -- Try to lock the remote machine:
	    -- TODO FIXME -- set up exception handler so that we UNLOCK the machine if we run into an error.
	    b <- lockRemote
	    if not b then do
              putStrLn$ " !! FAILED to lock remote machine "++host++", moving on..."
	      loop 
             else do {
	      putStrLn$ "\n  * Running config on idle machine: "++host;

	      -- FIXME: Awkward: We fork before we know if we really need to.
	      forkWithExceptions forkIO "configuration runner" $ do { 
		mytid <- myThreadId;
		myCompletion <- newEmptyMVar;

                -- Loop to find a viable configuration for the current worker:
                let grabConf = do {
		   -- Try to grab a configuration for this thread to work on:
		   -- Grabbing the configuration and adding ourselves to
		   -- the list must happen together, atomically:
		   mconf <- atomicModifyIORef state (\ st -> 
			     case st of 
			       ([],_)            -> (st,Nothing)
			       (conf:rest, tids) -> ((rest, M.insert mytid myCompletion tids),
						     Just conf));
		   (case mconf of 
		     Nothing   -> return ()
		     Just conf@(OneRunConfig conf_tries confls) -> do
		       let Just confindex = elemIndex confls (map binds allconfs)
		       putStrLn$ " ** Selected configuration "++ show (confindex+1)
			      ++" of "++show (length allconfs)++": " ++ show conf

                       let perconfD = perConfDirName conf
                           conf_outdir  = logd </> perconfD 
                           conf_workdir = tmpd </> perconfD 
		           -- .log file is at the same level as the conf-specific directory:
		           logfile = dropTrailingPathSeparator conf_outdir <.> "log"

                       b <- doesFileExist logfile
                       
                       if b then do 
                          putStrLn$ "  *!* CONFIGURATION ALREADY FINISHED: "++perconfD
                          putStrLn$ "    Proceeding to next configuration..."
                          grabConf
                        else do 
			 -- Create the output directory locally on this machine:
                         createDirectoryIfMissing True logd

			 -- Create the working directory and perform the git clone:
			 stillgood <- setupRemoteWorkingDirectory exec gitloc conf_workdir
                         -- At this point the above remote command has completed, and things are setup to run.
 
			 -- This is it.  Here we run the remote benchmark suite:
			 cmdoutput <- if stillgood then 
					runcmd (buildTheCommand finalcommand conf gitloc conf_workdir conf_outdir)
				      else 
					return Nothing

			 putStrLn$ "  * Remote command finished for config "++show (confindex+1)
			 -- Now we're done with all remote operations and can unlock:
			 unlockRemote

			 case cmdoutput of 
			   Nothing -> do putStrLn$ "  ! Config failed. "
					 -- Delete the directory with erroneous output:
					 putStrLn$ "  ! Deleting output directory: "++conf_outdir
					 b <- doesDirectoryExist  conf_outdir 
					 when b $ removeDirectory conf_outdir 
					 b <- doesDirectoryExist  conf_workdir
					 when b $ removeDirectory conf_workdir
					 if conf_tries+1 >= max_config_tries then do
					   putStrLn$ "  ! Conf failed the maximum number of times! ("++show max_config_tries++")"
					   atomicModifyIORef_ state  
					     (\ (confs,tids) -> (confs, M.delete mytid tids))
					  else do 
					   putStrLn$ "  ! Retrying config.  Try # "++ show (conf_tries + 2)
					   -- We put the config back and 
					   atomicModifyIORef_ state  
					     (\ (confs,tids) -> (OneRunConfig (conf_tries+1) confls:confs, 
								 M.delete mytid tids))
			   -- Normal completion, we leave ourselves in the thread list for our MVar to be blocked on.
			   -- We could also do a delete here, but there's no need.
			   Just bs -> do writeToLog logfile bs 
					 return ()
		      );
                    } -- End grabConf
                    in grabConf;

		putStrLn$ "  - Forked thread with ID " ++ show mytid ++ " completed.";
		putMVar myCompletion ();
	      }; -- End forkIO

	      -- Finally, keep going around the loop:
	      loop;
            }
	    -- end else

  loop -- Kick it off


-- FIXME
-- | Build the main command that runs the benchmarks.
buildTheCommand :: String -> OneRunConfig -> FullGitLoc -> String -> String -> [String]
buildTheCommand finalcmd (OneRunConfig _ conf) 
		         (FullGitLoc gitroot branch gitoffset)
			 remoteWorkingDir remoteOutputDir = 
  ["cd " ++ remoteWorkingDir </> gitoffset] ++
  map exportCmd (filter (isEnvVar  . fst) conf) ++
  [rtsOpts    (filter (isRTS     . fst) conf)] ++ 
  [ghcOpts    (filter (isCompile . fst) conf)] ++ 
  [finalcmd, 
   -- Finally retrieve results:
   "mkdir -p "++ remoteOutputDir,
   "touch dummy.log", "touch dummy.dat", -- This is silly... there must be a better way to avoid CP errors.
   "cp -f *.log *.dat "++ remoteOutputDir]
 where 
  envs = filter (isEnvVar . fst) conf
  exportCmd (EnvVar v,val) = ("export "++v++"=\""++val++"\"")

  -- These are interpreted in an EXTREMELY SIMPLE way right now.  The two strings are just appended:
  combineRTS  (RTS     flag, val) = flag++val
  combineFlag (Compile flag, val) = flag++val
  rtsOpts ls = exportCmd (EnvVar "GHC_RTS",   unwords (map combineRTS  ls))
  ghcOpts ls = exportCmd (EnvVar "GHC_FLAGS", unwords (map combineFlag ls))


-- | Setup a git clone as a fresh working directory.
setupRemoteWorkingDirectory (Executor{runcmd}) (FullGitLoc gitroot branch gitoffset) workingDir = do
  wd <- makeHomePathPortable workingDir
  -- Create the directory locally first:
--  runIO$ "mkdir -p "++wd -- Use HSH to handle ~/ path.
  x <- runcmd ["rm -rf "++wd,
               "mkdir -p "++wd, 
	       "git clone "++gitroot++" "++wd,
	       "cd "++wd,
	       "git checkout "++ branch,
	       "git submodule init",
	       "git submodule update"]
  case x of 
    Nothing -> do hPutStrLn stderr " ! Failed to setup git repo on remote worker node!"
		  return False
    Just _  -> do putStrLn$ "  - Done cloning remote working copy: " ++ wd
		  return True

--------------------------------------------------------------------------------

-- Create and return a NEW, UNUSED directory for storing logs during this run.
-- This is hardcoded to be inside $HOME/clusterbench/run_N : 
--   Returns a directory name.
findUnusedLogDir :: [BenchmarkSetting] -> IO Int
findUnusedLogDir settings = do
  -- Here is an inefficient system to find out what the next run should be:
  let loop n = do dir <- makeLogDir n 
                  b   <- doesDirectoryExist dir
		  if b then loop (n+1)
		       else return n
--  nextdir <- loop 1 
--  createDirectoryIfMissing True nextdir 
--  return nextdir
  loop 1 

-- Make a run_N log directory in the standardized place:
makeLogDir :: Int -> IO String 
makeLogDir n = do
  home <- getEnv "HOME"
  let root   = home </> "clusterbench"
      prefix = "run_"
      suffix = ""  -- TODO: add more interesting description based on what is *varied* in settings.
  return (root </> prefix ++ show n ++ suffix)

-- | Create a descriptive (and unused) directory based on a
-- configuration.  It will be a child of the logdir (i.e. the run_N
-- directory). This will be the destination for output results.
createPerConfDir :: String -> OneRunConfig -> IO String
createPerConfDir parentdir conf = do
  let descr = perConfDirName conf
      loop n = do
        let suffix = if n==0 then "" else "_"++show n
	    path = parentdir </> descr ++ suffix
	b <- doesFileExist path
	if b then loop (n+1)
	     else return path
  loop 0 >>= makeHomePathPortable

perConfDirName :: OneRunConfig -> String
perConfDirName (OneRunConfig _ conf) =
  intercalate "_" $ map paramPlainText conf


-- | Write out log file.
-- writeToLog :: String -> OneRunConfig -> B.ByteString -> IO String
-- writeToLog logd conf bytes = do
--  file <- confToFileName logd conf
--  let path = logd </> file
writeToLog path bytes = do
  path' <- dePortablize path
  B.writeFile path' bytes
  putStrLn$ "  - Wrote log output to file: " ++ path


-- Emit something descriptive for the option settings.
-- paramPlainText (p,v) = fn p ++ filter (not . isSpace) v
--  where 
--   fn (RTS s)     = s
--   fn (Compile s) = s
--   fn (EnvVar  s) = s

paramPlainText (RTS   "-s",_) = ""
paramPlainText (RTS     s,v) = clean s ++ clean v
paramPlainText (Compile s,v) = clean s ++ clean v
paramPlainText (EnvVar "GHC",v) = "GHC" ++ filter isNumber v
paramPlainText (EnvVar  s,v) = clean s ++ clean v
-- paramPlainText (EnvVar  s,v) = clean s

-- Remove characters we don't want in filenames:
clean = filter isAlphaNum


--------------------------------------------------------------------------------
--                              Main Script                                   --
--------------------------------------------------------------------------------

main = do
  -- First parse and check options:
  ---------------------------------
  argv <- getArgs
  let (options,args,errs) = getOpt Permute cli_options argv  

  unless (null errs) $ do 
    putStrLn$ "Errors parsing command line options:" 
    mapM_ (putStr . ("   "++)) errs
    putStr$ usageInfo "\nUsage: [options] configFile" cli_options
    exitFailure

  let isgit = Git `elem` options 

  -- Find idle machines:
  let machines = case filter isMachineList options of 
		   [] -> ["rrnewton@tank.cs.indiana.edu"]
		   ls -> concat $ map unMachineList ls

  -- If we're in ListIdle mode we do that and exit:
  when (ListIdle `elem` options) $ do 
    putStrLn$ "Polling: " ++ unwords machines
    putStrLn$ "All currently Idle machines (no one logged in):"
    putStrLn$ "==============================================="
    idles <- pollIdles machines
    mapM_ putStrLn idles
    exitSuccess

  -- Otherwise proceed to read in a config file:
  configFile  <- case args of 
    []  -> error$ "must take a config file"
    [f] -> return f
    ls  -> error$ "Error, extra arguments to script not understood: " ++ unwords ls

  ------------------------------------------------------------
  -- Now the core actions:

  putStrLn$ "Reading benchmark config from file: "++configFile++":"
  settings <- fmap readConfig$ readFile configFile
  mapM_ (print . nest 4 . pPrint) settings
  let numconfs = countConfigs settings
  putStrLn$ "Size of configuration space: "++ show numconfs

  putStrLn "Getting current directory relative to home dir:" 
  startpath <- getPortableWD 
  putStrLn$ "    "++startpath

  if isgit then do
    gitloc <- case filter isGitRepo settings of 
               [] -> do putStrLn$ "No explicit GitRepo specified.  Using working directory."
                        putStrLn$ "  Finding root of .git working copy containing ./ ..."
--		     FullGitLoc url0 branch0 workingdir <- findGitRoot
		        findGitRoot
	             
               [GitRepo url branch wd] -> do putStrLn$ "Using git repo/branch: "++ show (url,branch)
                                             return (FullGitLoc url branch wd)
	       ls  -> error$ "More than one GitRepo specified!"++show ls
    putStrLn$ "    "++show gitloc

    -- Grab idle machines as a stream of workers:
    idleMachines <- idleExecutors machines    

    -- Find where our output run should go (e.g. clusterbench/run_N folder):
    run_N <- case filter isRedoRun options of 
              [] -> findUnusedLogDir settings 
	      [RedoRun n] -> return n 
	      ls -> error$ "More than one --retry flag given!"

    launchConfigs run_N settings idleMachines gitloc startpath
    return ()

   else do 
    error "Non --git mode not implemented."
    return ()


