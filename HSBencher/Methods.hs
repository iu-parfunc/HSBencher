{-# LANGUAGE NamedFieldPuns #-}

-- | These are the built-in build methods for HSBencher.

module HSBencher.Methods
       (makeMethod, ghcMethod, cabalMethod,        
        )
       where

import Control.Monad
import System.Process
import System.Directory
import System.FilePath
import Text.Printf

import HSBencher.Types
import HSBencher.MeasureProcess

--------------------------------------------------------------------------------
-- General utilities
--------------------------------------------------------------------------------


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
  , compile = \ flags target -> do
     isdir <- doesDirectoryExist target
     let dir = if isdir then target
               else takeDirectory target
     system "make"
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
  , compile = \ flags target ->
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
  , compile = \ flags target -> do
     dir <- getDir target
     inDirectory dir $ do 
       -- Ugh... how could we separate out args to the different phases of cabal?
       system "rm -rf ./bin/*"
       let cmd = "cabal install --bindir=./bin/ ./ "++unwords flags
       putStrLn$ "RUNNING CMD "++cmd
       system cmd
       ls <- filesInDir "./bin/"
       case ls of
         []  -> error$"No binaries were produced from building cabal file! In: "++show dir
         [f] -> return (StandAloneBinary$ dir </> "bin" </> f)
         _   -> error$"Multiple binaries were produced from building cabal file!:"
                       ++show ls ++" In: "++show dir
  }
 where dotcab = WithExtension ".cabal"


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

inDirectory :: FilePath -> IO a -> IO a
inDirectory dir act = do 
  orig <- getCurrentDirectory
  setCurrentDirectory dir
  x <- act
  setCurrentDirectory orig
  return x
  
-- Returns actual files only
filesInDir d = do
  inDirectory d $ do
    ls <- getDirectoryContents "."
    filterM doesFileExist ls
