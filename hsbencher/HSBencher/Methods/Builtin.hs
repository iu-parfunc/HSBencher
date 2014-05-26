{-# LANGUAGE NamedFieldPuns #-}

-- | These are the built-in build methods for HSBencher that come with the main
-- package.  They are relatively unsophisticated.

module HSBencher.Methods.Builtin
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
import HSBencher.Internal.Logging (log)
import HSBencher.Internal.MeasureProcess
import HSBencher.Internal.Utils (runLogged, defaultTimeout)

--------------------------------------------------------------------------------
-- Some useful build methods
--------------------------------------------------------------------------------

-- | Build with GNU Make.  This is a basic Make protocol; your application may need
--   something more complicated.  This assumes targets clean, run, and the default
--   target for building.
--
--   The variables RUN_ARGS and COMPILE_ARGS are used to pass in the per-benchmark
--   run and compile options, so the Makefile must be written with these conventions
--   in mind.  Note that this build method never knows where or if any resulting
--   binaries reside.  One effect of that is that this simple build method can never
--   be used for PARALLEL compiles, because it cannot manage where the
--   build-intermediates are stored.
-- 
makeMethod :: BuildMethod
makeMethod = BuildMethod
  { methodName = "make"
  , canBuild = (IsExactly "Makefile")
               `PredOr`
               InDirectoryWithExactlyOne (IsExactly "Makefile")
  , concurrentBuild = False
  , setThreads      = Nothing
  , clean = \ pathMap _ target -> do
     doMake pathMap target $ \ makePath -> do
       _ <- runSuccessful subtag (makePath++" clean")
       return ()
  , compile = \ pathMap bldid flags target -> do
     doMake pathMap target $ \ makePath -> do
       absolute <- liftIO getCurrentDirectory
       _ <- runSuccessful subtag (makePath++" COMPILE_ARGS='"++ unwords flags ++"'")
       log$ tag++"Done building with Make, assuming this benchmark needs to run in-place..."
       let runit args envVars =
             CommandDescr
             { command = ShellCommand (makePath++" run RUN_ARGS='"++ unwords args ++"'")
             , timeout = Just defaultTimeout
             , workingDir = Just absolute
             , envVars
             }
       return (RunInPlace runit)
  }
 where
  tag = " [makeMethod] "
  subtag = " [make] "
  doMake pathMap target action = do
     isdir <- liftIO$ doesDirectoryExist target
     let dir = if isdir then target
               else takeDirectory target
         makePath = M.findWithDefault "make" "make" pathMap
     inDirectory dir (action makePath)


-- | Build with GHC directly.  This assumes that all dependencies are installed and a
-- single call to @ghc@ can build the file.
-- 
-- Compile-time arguments go directly to GHC, and runtime arguments directly to the
-- resulting binary.
ghcMethod :: BuildMethod
ghcMethod = BuildMethod
  { methodName = "ghc"
  , canBuild = WithExtension ".hs"
  , concurrentBuild = True -- Only if we use hermetic build directories.
  , setThreads = Just $ \ n -> [ CompileParam "-threaded -rtsopts"
                               , RuntimeParam ("+RTS -N"++ show n++" -RTS")]
  -- , needsInPlace = False
  , clean = \ pathMap bldid target -> do
     let buildD = "buildoutput_" ++ bldid
     liftIO$ do b <- doesDirectoryExist buildD
                when b$ removeDirectoryRecursive buildD
     return ()
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
--   Specifically, this uses "cabal install".
-- 
-- This build method attempts to choose reasonable defaults for benchmarking.  It
-- takes control of the output program suffix and directory (setting it to BENCHROOT/bin).
-- It passes compile-time arguments directly to cabal.  Likewise, runtime arguments
-- get passed directly to the resulting binary.
cabalMethod :: BuildMethod
cabalMethod = BuildMethod
  { methodName = "cabal"
   -- TODO: Add methodDocs
  , canBuild = dotcab `PredOr`
               InDirectoryWithExactlyOne dotcab
  , concurrentBuild = True
  , setThreads = Just $ \ n -> [ CompileParam "--ghc-option='-threaded' --ghc-option='-rtsopts'"
                               , RuntimeParam ("+RTS -N"++ show n++" -RTS")]
  , clean = \ pathMap _ target -> do
     return ()
  , compile = \ pathMap bldid flags target -> do

     benchroot <- liftIO$ getCurrentDirectory
     let suffix = "_"++bldid
         cabalPath = M.findWithDefault "cabal" "cabal" pathMap
         ghcPath   = M.findWithDefault "ghc" "ghc" pathMap
         binD      = benchroot </> "bin"
     liftIO$ createDirectoryIfMissing True binD

     dir <- liftIO$ getDir target -- Where the indiv benchmark lives.
     inDirectory dir $ do 
       let tmpdir = benchroot </> dir </> "temp"++suffix
       _ <- runSuccessful tag $ "rm -rf "++tmpdir
       _ <- runSuccessful tag $ "mkdir "++tmpdir

       -- Ugh... how could we separate out args to the different phases of cabal?
       log$ tag++" Switched to "++dir++", and cleared temporary directory."
       let extra_args  = "--bindir="++tmpdir++" ./ --program-suffix="++suffix
           extra_args' = if ghcPath /= "ghc"
                         then extra_args -- ++ " --with-ghc='"++ghcPath++"'"
                         else extra_args
       let cmd = cabalPath++" install "++ extra_args' ++" "++unwords flags
       log$ tag++"Running cabal command: "++cmd
       _ <- runSuccessful tag cmd
       -- Now make sure we got exactly one binary as output:
       ls <- liftIO$ filesInDir tmpdir
       case ls of
         [f] -> do _ <- runSuccessful tag$ "mv "++tmpdir++"/"++f++" "++binD++"/" -- TODO: less shelling
                   return (StandAloneBinary$ binD </> f)
         []  -> error$"No binaries were produced from building cabal file! In: "++show dir
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
-- Returns lines of output if successful.
runSuccessful :: String -> String -> BenchM [B.ByteString]
runSuccessful tag cmd = do
  (res,lines) <- runLogged tag cmd
  case res of
    ExitError code  -> error$ "expected this command to succeed! But it exited with code "++show code++ ":\n  "++ cmd
    RunTimeOut {}   -> error "Methods.hs/runSuccessful - internal error!"
    RunCompleted {} -> return lines
