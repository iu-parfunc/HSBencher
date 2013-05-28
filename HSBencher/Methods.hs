{-# LANGUAGE NamedFieldPuns #-}

-- | These are the built-in build methods for HSBencher.

module HSBencher.Methods
       (makeMethod, ghcMethod, cabalMethod,        
        )
       where

import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
-- import Control.Monad.IO.Class (liftIO, MonadIO)
import System.Process
import System.Directory
import System.FilePath
import Text.Printf
import Prelude hiding (log)

import HSBencher.Types
import HSBencher.Logging (log)
import HSBencher.MeasureProcess
import HSBencher.Utils (runLogged)

--------------------------------------------------------------------------------
-- Some useful build methods
--------------------------------------------------------------------------------

-- | Build with GNU Make.
makeMethod :: BuildMethod
makeMethod = BuildMethod
  { methodName = "make"
  , canBuild = (IsExactly "Makefile")
               `PredOr`
               InDirectoryWithExactlyOne (IsExactly "Makefile")
  , concurrentBuild = False
  , compile = \ bldid flags target -> do
     isdir <- liftIO$ doesDirectoryExist target
     let dir = if isdir then target
               else takeDirectory target
     _ <- runSuccessful " [make] " "make"
     let runit args =
           CommandDescr
           { command = RawCommand "make" ["run","ARGS=\""++ unwords args ++"\""]
           , timeout = Just 150  
           , workingDir = Just dir
           , envVars = []
           }
     return (RunInPlace runit)
  }

-- | Build with GHC directly.
ghcMethod :: BuildMethod
ghcMethod = BuildMethod
  { methodName = "ghc"
  , canBuild = WithExtension ".hs"
  , concurrentBuild = True -- Only if we use hermetic build directories.
  , compile = \ bldid  flags target -> liftIO$ 
     let dir  = takeDirectory target
         file = takeBaseName target in 
     inDirectory dir $ do
       let buildD = "buildoutput_" ++ bldid

-- 	 flags = flags_ ++ " -fforce-recomp -DPARSCHED=\""++ (schedToModule sched) ++ "\""         
     -- code1 <- lift$ system$ "mkdir -p "++outdir
     -- code2 <- lift$ system$ "mkdir -p "++exedir
--       args = if shortrun then shortArgs args_ else args_           
              
       system$ printf "ghc %S -outputdir ./%s %s"
               target buildD (unwords flags)
       return (StandAloneBinary$ dir </> buildD </> file)
  }

-- | Build with cabal.
cabalMethod :: BuildMethod
cabalMethod = BuildMethod
  { methodName = "cabal"
  , canBuild = dotcab `PredOr`
               InDirectoryWithExactlyOne dotcab
  , concurrentBuild = True
  , compile = \ bldid flags target -> do
     let suffix = "_"++bldid
     dir <- liftIO$ getDir target
     inDirectory dir $ do 
       -- Ugh... how could we separate out args to the different phases of cabal?
       log$ tag++" Switched to "++dir++", clearing binary target dir... "
       _ <- runSuccessful tag "rm -rf ./bin/*"
       let cmd = "cabal install --bindir=./bin/ ./ --program-suffix="++suffix++" "++unwords flags
       log$ tag++"Running cabal command: "++cmd
       _ <- runSuccessful " [cabal] " cmd
       ls <- liftIO$ filesInDir "./bin/"
       case ls of
         []  -> error$"No binaries were produced from building cabal file! In: "++show dir
         [f] -> return (StandAloneBinary$ dir </> "bin" </> f)
         _   -> error$"Multiple binaries were produced from building cabal file!:"
                       ++show ls ++" In: "++show dir
                       
  }
 where
   dotcab = WithExtension ".cabal"
   tag = " [cabalMethod] "

--------------------------------------------------------------------------------
-- Helper routines:
--------------------------------------------------------------------------------

-- | Checks whether a `BuildMethod` works for a given file
-- matchesMethod :: BuildMethod -> FilePath -> IO Bool
-- matchesMethod BuildMethod{canBuild} path =
--   return $ filePredCheck canBuild path

-- | Our compilation targets might be either directories or file names.
getDir :: FilePath -> IO FilePath
getDir path = do
  b  <- doesDirectoryExist path
  b2 <- doesFileExist path
  if b
    then return path
    else if b2
         then return (takeDirectory path)
         else error$ "getDir: benchmark target path does not exist at all: "++path

inDirectory :: (MonadIO m) => FilePath -> m a -> m a
inDirectory dir act = do 
  orig <- liftIO$ getCurrentDirectory
  liftIO$ setCurrentDirectory dir
  x <- act
  liftIO$ setCurrentDirectory orig
  return x
  
-- Returns actual files only
filesInDir :: FilePath -> IO [FilePath]
filesInDir d = do
  inDirectory d $ do
    ls <- getDirectoryContents "."
    filterM doesFileExist ls


-- | A simple wrapper for a command that is expected to succeed (and whose output we
-- don't care about).  Throws an exception if the command fails.
runSuccessful :: String -> String -> BenchM [B.ByteString]
runSuccessful tag cmd = do
  (res,lines) <- runLogged tag cmd
  case res of
    ExitError code  -> error$ "expected command to succeed! But it exited with code "++show code++ ": "++ cmd
    TimeOut {}      -> error "Methods.hs/runSuccessful - internal error!"
    RunCompleted {} -> return lines
