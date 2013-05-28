{-# LANGUAGE NamedFieldPuns #-}

-- | These are the built-in build methods for HSBencher.

module HSBencher.Methods
       (makeMethod, ghcMethod, cabalMethod,        
        )
       where

import Control.Monad
import Control.Monad.Reader
import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
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
  , compile = \ pathMap bldid flags target -> do
     isdir <- liftIO$ doesDirectoryExist target
     let dir = if isdir then target
               else takeDirectory target
         makePath = M.findWithDefault "make" "make" pathMap
     inDirectory dir $ do
       absolute <- liftIO getCurrentDirectory
       _ <- runSuccessful tag (makePath++" COMPILE_ARGS='"++ unwords flags ++"'")
       log$ tag++"Done building with Make, assuming this benchmark needs to run in-place..."
       let runit args =
             CommandDescr
             { command = ShellCommand (makePath++" run RUN_ARGS='"++ unwords args ++"'")
             , timeout = Just 150  
             , workingDir = Just absolute
             , envVars = []
             }
       return (RunInPlace runit)
  }
 where
  tag = " [makeMethod] "

-- | Build with GHC directly.
ghcMethod :: BuildMethod
ghcMethod = BuildMethod
  { methodName = "ghc"
  , canBuild = WithExtension ".hs"
  , concurrentBuild = True -- Only if we use hermetic build directories.
  , compile = \ pathMap bldid flags target -> do
     let dir  = takeDirectory target
         file = takeBaseName target
         suffix = "_"++bldid
         ghcPath = M.findWithDefault "ghc" "ghc" pathMap
     log$ tag++" Building target with GHC method: "++show target  
     inDirectory dir $ do
       let buildD = "buildoutput_" ++ bldid
       liftIO$ createDirectoryIfMissing True buildD
       let dest = buildD </> file ++ suffix
       runSuccessful " [ghc] " $
         printf "%s %s -outputdir ./%s -o %s %s"
           ghcPath file buildD dest (unwords flags)
       -- Consider... -fforce-recomp  
       return (StandAloneBinary$ dir </> dest)
  }
 where
  tag = " [ghcMethod] "

-- | Build with cabal.
cabalMethod :: BuildMethod
cabalMethod = BuildMethod
  { methodName = "cabal"
  , canBuild = dotcab `PredOr`
               InDirectoryWithExactlyOne dotcab
  , concurrentBuild = True
  , compile = \ pathMap bldid flags target -> do
     let suffix = "_"++bldid
         cabalPath = M.findWithDefault "cabal" "cabal" pathMap
         ghcPath   = M.findWithDefault "ghc" "ghc" pathMap
     dir <- liftIO$ getDir target
     inDirectory dir $ do 
       -- Ugh... how could we separate out args to the different phases of cabal?
       log$ tag++" Switched to "++dir++", clearing binary target dir... "
       _ <- runSuccessful tag "rm -rf ./bin/*"
       let extra_args  = "--bindir=./bin/ ./ --program-suffix="++suffix
           extra_args' = if ghcPath /= "ghc"
                         then extra_args -- ++ " --with-ghc='"++ghcPath++"'"
                         else extra_args
       let cmd = cabalPath++" install "++ extra_args' ++" "++unwords flags
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
-- TODO: Use bracket, but it's only IO, not generalized:
  -- bracket (do o <- liftIO getCurrentDirectory
  --             setCurrentDirectory dir
  --             return o)
  --         (\orig -> liftIO$ setCurrentDirectory orig)
  --         (\_ -> act)
  
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
    ExitError code  -> error$ "expected this command to succeed! But it exited with code "++show code++ ":\n  "++ cmd
    TimeOut {}      -> error "Methods.hs/runSuccessful - internal error!"
    RunCompleted {} -> return lines
