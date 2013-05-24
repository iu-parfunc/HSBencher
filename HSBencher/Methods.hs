{-# LANGUAGE NamedFieldPuns #-}

-- | These are the built-in build methods for HSBencher.

module HSBencher.Methods
       (makeMethod, ghcMethod, cabalMethod
--        matchesMethod
        )
       where

import HSBencher.Types

makeMethod :: BuildMethod
makeMethod = BuildMethod
  { methodName = "make"
  , canBuild = (IsExactly "Makefile")
               `PredOr`
               InDirectoryWithExactlyOne (IsExactly "Makefile")
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



