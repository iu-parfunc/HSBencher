{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, NamedFieldPuns, CPP #-}

-- Benchmark different configurations over a cluster of machines.
--
-- This includes the ability to search for unused machines and run benchmarks there.
-- 
-- This script is a layer on top of benchmark.hs and file_away.hs.
-- It may need to be compiled due to problems with HSH and ghci [2012.01.14].


-- TODO: 
--  * Detect and handle error codes in remote commands.
--  * Add robustness to disconnection (finish strongSSH)
--  * Check "who" repeatedly rather than just at the outset.

import HSH
import HSH.ShellEquivs
import Control.Monad 
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
import Control.Concurrent
import System.Console.GetOpt
import System.Environment (getEnv, getArgs)
import System.Exit
import System.Process
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
--	  | ConfigFile String
 deriving (Show, Eq, Read)


-- | Benchmark configuration spaces.
data BenchmarkSetting =  
   -- Vary parameter:
   Vary ParamType [String]
   -- Set parameter across all runs:
 | Set ParamType String
 | Command String 
 deriving (Show, Eq, Read, Typeable)

instance Pretty BenchmarkSetting where
  pPrint x = text $ show x 


-- | Different types of parameters that may be set or varied.
data ParamType = RTS     String   -- e.g. "-A"
	       | Compile String   -- e.g. "--ddump-simpl"
               | EnvVar  String
 deriving (Show, Eq, Read)


-- | The full configuration (list of parameter settings) for a single run.
type OneRunConfig = [(ParamType,String)]


-- | An abstract executor for remote commands.
data Executor = Executor 
  { host :: String, 
    -- Remote execution of a series of shell commands:
    -- Returns shell output if successfull.
    runcmd :: [String] -> IO (Maybe B.ByteString),
    lockRemote   :: IO Bool,
    unlockRemote :: IO ()
  }

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
listConfigs settings = loop constant varies  
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
  ids <- mapM (forkIO . wrap) comps
  -- And a timeout thread:
  forkIO $ do threadDelay (floor$ s * 1000 * 1000)
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
  who :: String <- run $ mkSSHCmd host ["who"] -|- grepV user
  return (null who)


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
findGitRoot :: IO (String,String)
findGitRoot = 
   do start <- runSL "pwd -L" 
      root  <- loop start
      canonA <- canonicalizePath root
      canonB <- canonicalizePath start
      let offset = makeRelative canonA canonB
      portroot <- makeHomePathPortable root
      return (portroot,offset)
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

  forkIO $ loop 
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
     ExitSuccess -> do forkIO $ putStr err  
		       return (Just$ B.pack out)
     ExitFailure _ -> return Nothing
#endif


-- | Construct a command string that will ssh into another machine and
--   run a series of commands.
mkSSHCmd :: String -> [String] -> String
  -- With no arguments it just makes sure that we can login:
mkSSHCmd host []    = "ssh "++host++" ' ' "
mkSSHCmd host cmds  = "ssh "++host++" '"++ concat (intersperse " && " cmds) ++ "'"


--------------------------------------------------------------------------------

-- | Job management.  Consume a stream of Idle Executors to schedule
--   jobs remotely.
launchConfigs :: [BenchmarkSetting] -> MVar Executor -> (String,String) -> String -> IO ()
launchConfigs settings idles (gitroot,gitoffset) startpath = do
  let allconfs = listConfigs settings
  -- If a configuration run fails, it fails on another thread so we
  -- need a mutable structure to keep track of them.
  --
  -- This atomically modified state tracks the confs and outstanding
  -- threads (each of which is modeled by an mvar that will be written
  -- at completion.).
  state <- newIORef (allconfs, M.empty)
  logd  <- makeLogDir settings  

  -- The main job scheduling loop:
  let finalcommand = getCommand settings
      loop = do
         peek <- readIORef state 
         case peek of 
     	   ([],m) | M.null m -> putStrLn$ "Done with all configurations.  All jobs finished." 
     	   ([],m) -> do -- Here we atomically discharge the obligation to wait for outstanding workers.
		        mvs <- atomicModifyIORef state 
			        (\ (confs,mp) -> 
				  case confs of 
				   []  -> (([], M.empty), M.elems mp)
				   _   -> ((confs, mp), []))
			putStrLn$ "  - launchConfigs: All launched, waiting for "++
				  show (length mvs)++" outstanding jobs to complete or fail."
			mapM_ readMVar mvs
			loop
           _  -> do 

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
	      forkIO $ do { 
		mytid <- myThreadId;
		myCompletion <- newEmptyMVar;

		mconf <- atomicModifyIORef state (\ st -> 
			  case st of 
			    ([],_)            -> (st,Nothing)
			    (conf:rest, tids) -> ((rest, M.insert mytid myCompletion tids),
						  Just conf));

		(case mconf of 
		  Nothing   -> return ()
		  Just conf -> do
                    let confindex = show (1 + (fromJust$ elemIndex conf allconfs))
		    putStrLn$ " ** Selected configuration "++confindex
		           ++" of "++show (length allconfs)++": " ++ show conf

		    -- Create the output directory locally on this machine:
		    confdir <- createConfDir logd conf 
		    -- .log file is at the same level as the conf-specific directory:
		    let logfile = dropTrailingPathSeparator confdir <.> "log"
			workingDir = confdir </> "working_copy"

		    -- Perform the git clone:
		    setupRemoteWorkingDirectory exec gitroot workingDir  

                    -- This is it.  Here we run the remote benchmark suite:
		    cmdoutput <- runcmd (buildCommand finalcommand conf (gitroot,gitoffset) workingDir)
		    
                    putStrLn$ "  * Remote command finished for config "++confindex
                    -- Now we're done with all remote operations and can unlock:
		    unlockRemote

		    case cmdoutput of 
		      Nothing -> do putStrLn$ "  ! Config failed.  Retrying..."
				    -- Delete the directory with erroneous output:
				    putStrLn$ "  ! Deleting output directory: "++confdir
				    removeDirectory confdir 
				    atomicModifyIORef_ state  
				       (\ (confs,tids) -> (conf:confs, M.delete mytid tids))
		      Just bs -> do writeToLog logfile bs 
				    return ()
		);

		putStrLn$ "  - Forked thread with ID " ++ show mytid ++ " completed.";
		putMVar myCompletion ();
	      }; -- End forkIO

	      -- Finally, keep going around the loop:
	      loop;
            }
	    -- end else

  loop -- Kick it off


-- FIXME
buildCommand :: String -> OneRunConfig -> (String,String) -> String -> [String]
buildCommand finalcmd conf (gitroot,gitoffset) remoteWorkingDir = 
  ["cd " ++ remoteWorkingDir </> gitoffset] ++
  map exportCmd (filter (isEnvVar  . fst) conf) ++
  [rtsOpts    (filter (isRTS     . fst) conf)] ++ 
  [ghcOpts    (filter (isCompile . fst) conf)] ++ 
  [finalcmd]
 where 
  envs = filter (isEnvVar . fst) conf
  exportCmd (EnvVar v,val) = ("export "++v++"=\""++val++"\"")

  -- These are interpreted in an EXTREMELY SIMPLE way right now.  The two strings are just appended:
  combineRTS  (RTS     flag, val) = flag++val
  combineFlag (Compile flag, val) = flag++val
  rtsOpts ls = exportCmd (EnvVar "GHC_RTS",   unwords (map combineRTS  ls))
  ghcOpts ls = exportCmd (EnvVar "GHC_FLAGS", unwords (map combineFlag ls))



-- | Setup a git clone as a fresh working directory.
setupRemoteWorkingDirectory (Executor{runcmd}) gitroot workingDir = do
  wd <- makeHomePathPortable workingDir
  -- Create the directory locally first:
  runIO$ "mkdir -p "++wd -- Use HSH to handle ~/ path.
  runcmd ["mkdir -p "++wd, 
	  "git clone "++gitroot++" "++wd,
	  "cd "++wd,
	  "git submodule init",
	  "git submodule update"]
  putStrLn$ "  - Done cloning remote working copy:" ++ wd

--------------------------------------------------------------------------------

-- Create and return a directory for storing logs during this run.
makeLogDir :: [BenchmarkSetting] -> IO String 
makeLogDir settings = do
  home <- getEnv "HOME"
  let root   = home </> "clusterbench"
      prefix = "run_"
      suffix = ""  -- TODO: add more interesting description based on what is *varied* in settings.
--  existing :: [String] <- run$ "ls "++root </> prefix++"*"
--  let numexisting = length existing 
  -- Here is an inefficient system to find out what the next run should be:
  let loop n = do let dir = root </> prefix ++ show n ++ suffix
                  b <- doesDirectoryExist dir
		  if b then loop (n+1)
		       else return dir
  nextdir <- loop 1 
  createDirectoryIfMissing True nextdir 
  return nextdir

-- | Write out log file and return the file name used.
-- writeToLog :: String -> OneRunConfig -> B.ByteString -> IO String
-- writeToLog logd conf bytes = do
--  file <- confToFileName logd conf
--  let path = logd </> file
writeToLog path bytes = do
  path' <- dePortablize path
  B.writeFile path' bytes
  putStrLn$ "  - Wrote log output to file: " ++ path


-- | Create a descriptive (and unused) directory based on a
-- configuration.  It will be a child of the run_N directory. This
-- will be the destination for output results.
createConfDir :: String -> OneRunConfig -> IO String
createConfDir logd conf = do
  let descr = intercalate "_" $ map paramPlainText conf
      loop n = do
        let suffix = if n==0 then "" else "_"++show n
	    path = logd </> descr ++ suffix
	b <- doesFileExist path
	if b then loop (n+1)
	     else return path
  loop 0 >>= makeHomePathPortable

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
    putStrLn$ "Finding root of .git repository:"
    gitloc <- findGitRoot
    putStrLn$ "    "++show gitloc

    -- Grab idle machines as a stream of workers:
    idleMachines <- idleExecutors machines    

    launchConfigs settings idleMachines gitloc startpath
    return ()

   else do 
    error "Non --git mode not implemented."
    return ()


