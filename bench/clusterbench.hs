#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

-- Benchmark different configurations over a cluster of machines.
--
-- This includes the ability to search for unused machines and run benchmarks there.
-- 
-- This script is a layer on top of benchmark.hs and file_away.hs.
-- It may need to be compiled due to problems with HSH and ghci [2012.01.14].

import HSH
import HSH.ShellEquivs
import Control.Monad 
import Data.Maybe
import Data.Char (isSpace)
import Data.List
import Data.IORef
import Data.Typeable (Typeable, typeOf)
import Control.Concurrent
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Process
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.FilePath (dropTrailingPathSeparator, 
			addTrailingPathSeparator, takeDirectory, (</>))
import Text.Regex 
import Text.PrettyPrint.HughesPJClass (Pretty, pPrint, text)
import Text.PrettyPrint.HughesPJ (nest)
--------------------------------------------------------------------------------
-- Type definitions:

-- | Command line Flags.
data Flag = Git
          | MachineList [String]
--	  | ConfigFile String

 deriving (Show, Eq, Read)

-- | Benchmark configuration spaces.
data BenchmarkSetting =  
   -- Vary parameter:
   Vary ParamType [String]
   -- Set parameter across all runs:
 | Set ParamType String

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

--------------------------------------------------------------------------------

isMachineList (MachineList _) = True 
isMachineList _               = False
unMachineList (MachineList ls) = ls

cli_options :: [OptDescr Flag]
cli_options = 
     [ Option ['g'] ["git"] (NoArg Git) 
          "Assume the current directory is part of a clonable git working copy."
     , Option ['m'] ["machines"] 
          (ReqArg (MachineList . words) "HOSTS")
          "list of machines to use for running benchmarks"
--      , Option ['f'] ["config"] 
--           (ReqArg ConfigFile "FILE")
--           "read benchmark configuration from file"
     ]

machineIdle :: String -> IO Bool
machineIdle host = do 
  localUser <- getEnv "USER"
  let 
      remoteUser = parseUser host
      user = case remoteUser of 
                Nothing -> localUser
		Just s  -> s 

  who :: String <- run $ sshRun host ["who"] -|- grepV user
  return (null who)

------------------------------------------------------------
-- Configuration helpers

readConfig :: String -> [BenchmarkSetting]
readConfig s = 
  myread $ trim $ unlines $
  -- We allow comments in the file.
  -- (Scheme readers handle this by default.)
--  filter (not . isPrefixOf "--") $  map trim $ 
 -- Cannot get this to work:
--  filter (not . isJust . (matchRegex$ mkRegexWithOpts "^\\s*\\-\\-" True True)) $  
  filter (not . isCommentLine) $ 
--  filter (not . isJust . (matchRegex$ mkRegexWithOpts "\\-*\\-\\-" True True)) $  
  lines s 

isCommentLine :: String -> Bool
isCommentLine = isPrefixOf "--" . trim 

countConfigs :: [BenchmarkSetting] -> Int
countConfigs = undefined

--------------------------------------------------------------------------------
-- Misc Helpers:

newtype TimeOut = Seconds Double

-- | Fork a list of computations with a timeout.  Return an  that
-- will be asynchronously updated with the results.
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
	  -- There can no more than one response per computation:
	  take (length comps) ls)


-- | Construct a command string that will ssh into another machine and
--   run a series of commands.
sshRun :: String -> [String] -> String
-- This just makes sure that we can login:
sshRun host []    = "ssh "++host++" ' ' "
sshRun host cmds  = "ssh "++host++" '"++ concat (intersperse " && " cmds) ++ "'"

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

-- | Return a portable "~/foo" path describing the location of the git
--   working copy root.  This assumes that there is a parent directory
--   containing a ".git"
findGitRoot :: IO String
findGitRoot = 
   runSL "pwd -L" >>= loop 
 where 
  loop path = do 
--    putStrLn$ "Checking "++ path </> ".git"
    b0 <- isHomePath path 
    unless b0 $ do
       putStrLn$ "ERROR, findGitRoot: descended out of the users home directory to: "++ path
       exitFailure 
    b <- doesDirectoryExist$ path </> ".git"
    if b then makeHomePathPortable path
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


-- myread str = helper undefined str
--  where 
--   helper dummy str = 
--     let typ = show (typeOf dummy) in
--     case reads str of 
--       [] -> error$ "read: Could not read as type "++typ++": "++ str
--       (x,""):_   -> x
--       (_,ext):_  -> error$ "read: valid parse as type "++typ++"but extra characters at the end: "++ ext
--       []         -> dummy

--------------------------------------------------------------------------------
-- Main Script

main = do
  ------------------------------------------------------------
  -- First parse and check options:

  argv <- getArgs
  let (options,args,errs) = getOpt Permute cli_options argv  

  unless (null errs) $ do 
    putStrLn$ "Errors parsing command line options:" 
    mapM_ (putStr . ("   "++)) errs
    putStr$ usageInfo "\nUsage: [options] configFile" cli_options
    exitFailure

  -- Find idle machines:
  let machines = case filter isMachineList options of 
		   [] -> ["rrnewton@tank.cs.indiana.edu"]
		   ls -> concat $ map unMachineList ls
  configFile  <- case args of 
    []  -> error$ "must take a config file"
    [f] -> return f
    ls  -> error$ "Error, extra arguments to script not understood: " ++ unwords ls

  let isgit = Git `elem` options 

  ------------------------------------------------------------

  putStrLn$ "Reading benchmark config from file: "++configFile++":"
  config <- fmap readConfig$ readFile configFile
--  print$ nest 4 $ pPrint config
  mapM_ (print . nest 4 . pPrint) config

  ------------------------------------------------------------
  -- Search for idle Machines

  idleMachines <- fmap catMaybes $ 
  	          forkCmds (Seconds 10) $  
		  map (\s -> do idle <- machineIdle s
				return (if idle then Just s else Nothing)
		       ) machines

--  config <- case filter isConfigFile options

  putStr$ "IDLE machines:\n   "
  print idleMachines

  ------------------------------------------------------------

  putStrLn "Getting current directory relative to home dir:" 
  path <- getPortableWD 
  putStrLn$ "    "++path

  if isgit then do
    putStrLn$ "Finding root of .git repository:"
    gitroot <- findGitRoot
    putStrLn$ "    "++gitroot

   else do 
    return ()

  ------------------------------------------------------------



  ------------------------------------------------------------
  -- TEMP

--   forM_ idleMachines $ \ host -> do
--     putStrLn$ "Doing listing on analogous directory on machine "++ host
--     runIO$ sshRun host ["cd "++path, "ls"]

  return ()



