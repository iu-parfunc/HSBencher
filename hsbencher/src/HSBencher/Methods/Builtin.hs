{-# LANGUAGE NamedFieldPuns #-}

-- | These are the built-in build methods for HSBencher that come with the main
-- package.  They are relatively unsophisticated.

module HSBencher.Methods.Builtin
       (makeMethod, ghcMethod, cabalMethod,
        )
       where

import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import System.Process
import System.Directory
import System.FilePath
import Text.Printf
import Prelude hiding (log)

import HSBencher.Types
import HSBencher.Internal.Logging (log)
import HSBencher.Internal.Utils (runLogged)

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
       _ <- runSuccessful subtag (makePath++" clean") []
       return ()
  , compile = \ Config{pathRegistry, runTimeOut} _bldid flags buildenv target -> do
     doMake pathRegistry target $ \ makePath -> do
       absolute <- liftIO getCurrentDirectory
       _ <- runSuccessful subtag (makePath++" COMPILE_ARGS='"++ unwords flags ++"'") buildenv
       log$ tag++"Done building with Make, assuming this benchmark needs to run in-place..."
       -- Creating a runit function, that can be used to run
       -- the now compiled benchmark.
       let runit args envVars =
             CommandDescr
             { command = ShellCommand (makePath++" run RUN_ARGS='"++ unwords args ++"'")
             , timeout = runTimeOut
             , workingDir = Just absolute
             , tolerateError = False
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
  , clean = \ _cfg bldid _target -> do
     let buildD = "buildoutput_" ++ bldid
     liftIO$ do b <- doesDirectoryExist buildD
                when b$ removeDirectoryRecursive buildD
     return ()
  , compile = \ Config{pathRegistry} bldid flags buildEnv target  -> do
     let dir  = takeDirectory target
         file = takeBaseName target
         suffix = "_"++bldid
         ghcPath = M.findWithDefault "ghc" "ghc" pathRegistry
     log$ tag++" Building target with GHC method: "++show target
     inDirectory dir $ do
       let buildD = "buildoutput_" ++ bldid
       liftIO$ createDirectoryIfMissing True buildD
       let dest = buildD </> file ++ suffix
       _ <- runSuccessful " [ghc] "
         (printf "%s %s -outputdir ./%s -o %s %s"
            ghcPath file buildD dest (unwords flags)) buildEnv
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
  , clean = \ _ _ _target -> return ()
  , compile = \ Config{pathRegistry} bldid flags buildEnv target -> do

     benchroot <- liftIO$ getCurrentDirectory
     let suffix = "_"++bldid
         cabalPath = M.findWithDefault "cabal" "cabal" pathRegistry
         _ghcPath  = M.findWithDefault "ghc"   "ghc"   pathRegistry
         binD      = benchroot </> "bin"
     liftIO$ createDirectoryIfMissing True binD

     dir <- liftIO$ getDir target -- Where the indiv benchmark lives.
     inDirectory dir $ do
       let tmpdir = benchroot </> dir </> "temp"++suffix
       -- Env should not matter here
       _ <- runSuccessful tag ("rm -rf "++tmpdir) []
       _ <- runSuccessful tag ("mkdir "++tmpdir)  []

       -- Ugh... how could we separate out args to the different phases of cabal?
       log$ tag++" Switched to "++dir++", and cleared temporary directory."

       -- some extra printing (debugging Obsidian benchmarks)
       curr_dir <- liftIO$ getCurrentDirectory
       log$ tag++" Curently in directory: " ++ curr_dir
       let cmd0 = cabalPath++" install "++" "++unwords flags
           cmd1 = cmd0++" --only-dependencies"
           cmd2 = cmd0++" --bindir="++tmpdir++" ./ --program-suffix="++suffix
       log$ tag++"Running cabal command for deps only: "++cmd1
       _ <- runSuccessful tag cmd1 buildEnv
       log$ tag++"Running cabal command to build benchmark: "++cmd2
       _ <- runSuccessful tag cmd2 buildEnv
       -- Now make sure we got exactly one binary as output:
       ls <- liftIO$ filesInDir tmpdir
       case ls of
         [f] -> do _ <- runSuccessful tag ("mv "++tmpdir++"/"++f++" "++binD++"/") buildEnv -- TODO: less shelling
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
runSuccessful :: String -> String -> EnvVars -> BenchM [B.ByteString]
runSuccessful tag cmd env = do
  (res,lns) <- runLogged tag cmd env
  case res of
    ExitError { errcode} -> error$ "expected this command to succeed! But it exited with code "
                            ++show errcode++ ":\n  "++ cmd
    RunTimeOut {}   -> error$ "Methods.hs/runSuccessful - error! The following command timed out:\n  "++show cmd
    RunCompleted {} -> return lns
