{-# LANGUAGE NamedFieldPuns #-}

-- | These are the built-in build methods for HSBencher.

module HSBencher.Methods
       (makeMethod, ghcMethod, cabalMethod,        
        )
       where

import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.Process
import System.Directory
import System.FilePath
import Text.Printf
import Prelude hiding (log)

import HSBencher.Types
import HSBencher.Logging (log)
import HSBencher.MeasureProcess


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
     liftIO$ system "make"
     let runit args =
           CommandDescr
           { exeFile = "make"
           , cmdArgs = ["run","ARGS=\""++ unwords args ++"\""]
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
       let buildD = "buildoutput_" ++ makeBuildID flags

-- 	 flags = flags_ ++ " -fforce-recomp -DPARSCHED=\""++ (schedToModule sched) ++ "\""         
       
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
     dir <- liftIO$ getDir target
     inDirectory dir $ do 
       -- Ugh... how could we separate out args to the different phases of cabal?
       log$ tag++" Switched to "++dir++", clearing binary target dir... "
       liftIO$ system "rm -rf ./bin/*"
       let cmd = "cabal install --bindir=./bin/ ./ "++unwords flags
       log$ tag++" Running cabal command: "++cmd
       liftIO$ system cmd -- TODO: run tagged
       ls <- liftIO$ filesInDir "./bin/"
       case ls of
         []  -> error$"No binaries were produced from building cabal file! In: "++show dir
         [f] -> return (StandAloneBinary$ dir </> "bin" </> f)
         _   -> error$"Multiple binaries were produced from building cabal file!:"
                       ++show ls ++" In: "++show dir
  }
 where
   dotcab = WithExtension ".cabal"
   tag = " [cabalDriver]"

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
