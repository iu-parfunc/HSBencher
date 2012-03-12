

-- Helper routines used by other scripts in this directory.

module ScriptHelpers
 ( trim
 , unlessM 
 , indent
 , runEcho
 , inDirectory
 ) 
where

import HSH
import Data.Char
import Control.Monad
import System.Directory

--------------------------------------------------------------------------------

-- remComments :: String -> [String] -> [String]
-- remComments commentchars ls = filter (pred . stripLeadingWhitespace) ls
--  where 
--   pred str = not (take (length commentchars) str == commentchars) 
--   stripLeadingWhitespace []      = [] 
--   stripLeadingWhitespace (' ':t) = stripLeadingWhitespace t
--   stripLeadingWhitespace ls      = ls

-- | Trim whitespace from both ends of a string.
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


inDirectory dir action = do
  d1 <- getCurrentDirectory
  setCurrentDirectory dir
  x <- action
  setCurrentDirectory d1
  return x

-- | Both run a command and echo it before runnning.
runEcho cmd = do putStrLn$ "  Running: "++ cmd
		 runIO $ cmd -|- indent

unlessM m1 m2 = do x <- m1; unless x m2

indent = ("   "++)
