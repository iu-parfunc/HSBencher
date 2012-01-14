#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

-- Benchmark different configurations over a cluster of machines.

-- This includes the ability to search for unused machines and run benchmarks there.

import HSH
import HSH.ShellEquivs
import Control.Monad 
import Data.Maybe
import Data.List
import Data.IORef
import Control.Concurrent
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Process
import Text.Regex (mkRegex, subRegex, matchRegexAll)
-- import Data.String.Utils (replace)
--------------------------------------------------------------------------------


-- Search through the machine list for an unused one.

data Flag = 
  MachineList [String]
 deriving Eq

isMachineList (MachineList _) = True 
-- isMachineList _               = False
unMachineList (MachineList ls) = ls

cli_options :: [OptDescr Flag]
cli_options = 
     [ 
--     Option ['V']     ["version"] (NoArg Version)   "show version number"
--       Option [] [""] (NoArg Copy)  ""
       Option ['m'] ["machines"] 
          (ReqArg (MachineList . words)
-- (\s -> MachineList$
--                          case x of Nothing -> []
--                          	   Just s  -> words s )
                  "HOSTS")
          "list of machines to use for running benchmarks"
     ]


machineIdle :: String -> IO Bool
machineIdle host = do 
  putStrLn$ "Searching for idle machines:"
  env <- getEnvironment

  let Just localUser = lookup "USER" env
      remoteUser = parseUser host
      user = case remoteUser of 
                Nothing -> localUser
		Just s  -> s 

  who :: String <- run $ sshRun host ["who"] -|- grepV user
  return (null who)

--------------------------------------------------------------------------------
-- Helpers:

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

-- Get a path of the form "~/foo" that will work on another machine.
getPortablePath :: IO String
getPortablePath = do
  logical <- runSL "pwd -L"    -- getCurrentDirectory won't cut it.
  home    <- getEnv "HOME"
  return$ subRegex (mkRegex home) logical "~/"
--  return$ replace home "~/" logical

-- | Return Just "user" given "user@host.com".
parseUser str = 
  case matchRegexAll (mkRegex ".*@") str of
     Nothing          -> Nothing
     Just (_,hit,_,_) -> Just (reverse$ tail$ reverse hit)

--------------------------------------------------------------------------------
-- Main Script

main = do

  argv <- getArgs
  let (options,args,errs) = getOpt Permute cli_options argv  

  unless (null errs) $ do 
    putStrLn$ "Errors parsing command line options:" 
    mapM_ (putStr . ("   "++)) errs
    putStr$ usageInfo "\nUsage: " cli_options
    exitFailure

  env <- getEnvironment
--  let Just user = lookup "USER" env

  let machines = case filter isMachineList options of 
		   [] -> ["rrnewton@tank.cs.indiana.edu"]
		   ls -> concat $ map unMachineList ls
  case args of 
    [] -> return ()
    ls -> error$ "Error, extra arguments to script not understood: " ++ unwords ls

  results <- forkCmds (Seconds 10) $ 
	     map (\s -> do idle <- machineIdle s
		           return (if idle then Just s else Nothing)
		  ) machines

  let idleMachines = catMaybes results

  print idleMachines

  putStrLn "Getting current directory relative to home dir:" 
  path <- getPortablePath 
  putStrLn path

  forM_ idleMachines $ \ host -> do
    putStrLn$ "Doing listing on analogous directory on machine "++ host
    runIO$ sshRun host ["cd "++path, "ls"]

  return ()



