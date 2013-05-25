{-# LANGUAGE NamedFieldPuns #-}

-- | These are the built-in build methods for HSBencher.

module HSBencher.Methods
       (makeMethod, ghcMethod, cabalMethod
--        matchesMethod
        )
       where

import Data.Char
import Data.List
import System.Process
import System.Directory
import System.FilePath
import Text.Printf

import HSBencher.Types
import HSBencher.MeasureProcess

--------------------------------------------------------------------------------
-- General utilities
--------------------------------------------------------------------------------

-- | A BuildID should uniquely identify a particular configuration, but consist only
-- of characters that would be reasonable to put ina filename.  This is used to keep
-- build results from colliding.
type BuildID = String

-- | The BuildID should 
makeBuildID :: CompileFlags -> BuildID
makeBuildID strs =
  intercalate "_" $
  map (filter charAllowed) strs
 where
  charAllowed = isAlphaNum

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
  , canBuild = WithExtension "hs"
  , concurrentBuild = True -- Only if we use hermetic build directories.
  , compile = \ flags target ->
     let dir  = takeDirectory target
         file = takeBaseName target in 
     inDirectory dir $ do
       let buildD = "buildoutput_" ++ makeBuildID flags
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
     error "FINISHME"
  }
 where dotcab = WithExtension "cabal"


-- | Checks whether a `BuildMethod` works for a given file
-- matchesMethod :: BuildMethod -> FilePath -> IO Bool
-- matchesMethod BuildMethod{canBuild} path =
--   return $ filePredCheck canBuild path


inDirectory dir act = do 
  orig <- getCurrentDirectory
  setCurrentDirectory dir
  x <- act
  setCurrentDirectory orig
  return x
  
