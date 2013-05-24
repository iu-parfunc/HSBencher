{-# LANGUAGE NamedFieldPuns #-}

-- | These are the built-in build methods for HSBencher.

module HSBencher.Methods
       (makeMethod, ghcMethod, cabalMethod
--        matchesMethod
        )
       where

import System.Process
import System.Directory
import System.FilePath
import HSBencher.Types
import HSBencher.MeasureProcess

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
           }
     return (RunInPlace runit)
  }

ghcMethod :: BuildMethod
ghcMethod = BuildMethod
  { methodName = "ghc"
  , canBuild = WithExtension "hs"
  }

cabalMethod :: BuildMethod
cabalMethod = BuildMethod
  { methodName = "cabal"
  , canBuild = dotcab `PredOr`
               InDirectoryWithExactlyOne dotcab
  }
 where dotcab = WithExtension "cabal"


-- | Checks whether a `BuildMethod` works for a given file
-- matchesMethod :: BuildMethod -> FilePath -> IO Bool
-- matchesMethod BuildMethod{canBuild} path =
--   return $ filePredCheck canBuild path



