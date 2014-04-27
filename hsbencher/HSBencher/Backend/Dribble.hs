{-# LANGUAGE RecordWildCards, TypeFamilies, NamedFieldPuns, DeriveDataTypeable #-}

-- | A simple backend that dribbles benchmark results (i.e. rows/tuples) into a
-- series of files in an "hsbencher" subdir of the the users ".cabal/" directory.
-- 
-- This is often useful as a failsafe to reinforce other backends that depend on
-- connecting to internet services for upload.  Even if the upload fails, you still
-- have a local copy of the data.

module HSBencher.Backend.Dribble 
       ( defaultDribblePlugin, 
         DribblePlugin(), DribbleConf{..}
       ) 
   where

import HSBencher.Types
import HSBencher.Logging (log, chatter)

import Control.Concurrent.MVar
import Control.Monad.Reader
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Typeable
import System.IO.Unsafe (unsafePerformIO)
import System.Directory 
import System.FilePath ((</>),(<.>), splitExtension)

import Prelude hiding (log)

--------------------------------------------------------------------------------

-- | A simple singleton type -- a unique signifier.
data DribblePlugin = DribblePlugin deriving (Read,Show,Eq,Ord)

-- | A plugin with the basic options (if any) included.
defaultDribblePlugin :: DribblePlugin
defaultDribblePlugin = DribblePlugin

-- | The configuration consists only of the location of a single file, which is where
-- the results will be fed.  If no file is provided, the default location is selected
-- during plugin initialization.
data DribbleConf = DribbleConf { csvfile :: Maybe String }
  deriving (Read,Show,Eq,Ord, Typeable)

-- TODO: expose command line option to change directory for dribbling.  This is not
-- urgent however, because the user can dig around and set the DribbleConf directly
-- if they wish.

--------------------------------------------------------------------------------

instance PlugIn DribblePlugin where
  -- | No configuration info for this plugin currently:
  type PlugConf DribblePlugin = DribbleConf
  -- | No command line flags either:
  type PlugFlag DribblePlugin = ()

  defaultPlugConf _ = DribbleConf { csvfile = Nothing }

  -- | Going with simple names, but had better make them unique!
  plugName _ = "dribble"
  -- plugName _ = "DribbleToFile_Backend"  

  plugCmdOpts _ = ("Dribble plugin loaded: (But it has no command line options.)",[])

  plugUploadRow p cfg row = runReaderT (uploadBenchResult row) cfg

  plugInitialize p gconf = do 
   putStrLn " [dribble] Dribble-to-file plugin initializing..."
   let DribbleConf{csvfile} = getMyConf DribblePlugin gconf
   case csvfile of 
     Just x -> do putStrLn$ " [dribble] Using dribble file specified in configuration: "++show x
                  return gconf
     Nothing -> do 
      cabalD <- getAppUserDataDirectory "cabal"
      chk1   <- doesDirectoryExist cabalD
      unless chk1 $ error $ " [dribble] Plugin cannot initialize, cabal data directory does not exist: "++cabalD 
      let dribbleD = cabalD </> "hsbencher"
      createDirectoryIfMissing False dribbleD
      base <- case benchsetName gconf of 
                Nothing -> do putStrLn " [dribble] no --name set, chosing default.csv for dribble file.." 
                              return "dribble"
                Just x  -> return x
      let path = dribbleD </> base <.> "csv"
      putStrLn $ " [dribble] Defaulting to dribble location "++show path++", done initializing."
      return $! setMyConf p (DribbleConf{csvfile=Just path}) gconf

  foldFlags p flgs cnf0 = cnf0

--------------------------------------------------------------------------------

-- TEMP: Hack
fileLock :: MVar ()
fileLock = unsafePerformIO (newMVar ())
-- TODO/FIXME: Make this configurable.

uploadBenchResult :: BenchmarkResult -> BenchM ()
uploadBenchResult  br@BenchmarkResult{..} = do
    let tuple = resultToTuple br
        (cols,vals) = unzip tuple
    conf <- ask
    let DribbleConf{csvfile} = getMyConf DribblePlugin conf
    case csvfile of
      Nothing -> error "[dribble] internal plugin error, csvfile config should have been set during initialization."
      Just path -> do 
        log$ " [dribble] Adding a row of data to: "++path
        lift $ withMVar fileLock $ \ () -> do 
           b  <- doesFileExist path
           -- If we're the first to write the file... append the header:
           unless b$ writeFile path (concat (L.intersperse "," cols)++"\n")
           appendFile path (concat (L.intersperse "," (map show vals))++"\n")
        return ()
