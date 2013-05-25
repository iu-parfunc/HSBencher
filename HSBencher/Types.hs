{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NamedFieldPuns, CPP  #-}

module HSBencher.Types
       (
         -- * Benchmark building
         RunFlags, CompileFlags, FilePredicate(..), filePredCheck,
         BuildResult(..), BuildMethod(..),
         
         -- * Benchmark configuration spaces
         Benchmark(..), BenchRun(..),
         Benchmark2(..), BenchSpace(..), ParamSetting(..),
         
         -- * HSBench Driver Configuration
         Config(..), BenchM,
         Sched(..)
       )
       where

import Control.Monad.Reader
import Data.Maybe (catMaybes)
import Control.Monad (filterM)
import System.FilePath
import System.Directory
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as B
import qualified System.IO.Streams as Strm

import HSBencher.MeasureProcess -- (CommandDescr(..))

#ifdef FUSION_TABLES
import Network.Google.FusionTables (TableId)
#endif

----------------------------------------------------------------------------------------------------
-- Benchmark Build Methods

type RunFlags     = [String]
type CompileFlags = [String]

-- | A description of a set of files.  The description may take one of multiple
-- forms.
data FilePredicate = 
    WithExtension String -- ^ E.g. ".hs", WITH the dot.
  | IsExactly     String -- ^ E.g. "Makefile"
--   | SatisfiesPredicate (String -> Bool)

  | InDirectoryWithExactlyOne FilePredicate
    -- ^ A common pattern.  For example, we can build a file foo.c, if it lives in a
    -- directory with exactly one "Makefile".

  | PredOr FilePredicate FilePredicate -- ^ Logical or.

  -- TODO: Allow arbitrary function predicates also.
 deriving Show    
-- instance Show FilePredicate where
--   show (WithExtension s) = "<FilePredicate: *."++s++">"    


-- | This function gives meaning to the `FilePred` type.
--   It returns a filepath to signal "True" and Nothing otherwise.
filePredCheck :: FilePredicate -> FilePath -> IO (Maybe FilePath)
filePredCheck pred path =
  let filename = takeFileName path in 
  case pred of
    IsExactly str     -> return$ if str == filename
                                 then Just path else Nothing
    WithExtension ext -> return$ if takeExtension filename == ext
                                 then Just path else Nothing
    PredOr p1 p2 -> do
      x <- filePredCheck p1 path
      case x of
        Just _  -> return x
        Nothing -> filePredCheck p2 path
    InDirectoryWithExactlyOne p2 -> do
      ls  <- getDirectoryContents =<< getCurrentDirectory
      ls' <- fmap catMaybes $
             mapM (filePredCheck p2) ls
      case ls' of
        [x] -> return (Just$ takeDirectory path </> x)
        _   -> return Nothing

-- instance Show FilePredicate where
--   show (WithExtension s) = "<FilePredicate: *."++s++">"  

-- | The result of doing a build.  Note that `compile` can will throw an exception if compilation fails.
data BuildResult =
    StandAloneBinary FilePath -- ^ This binary can be copied and executed whenever.
  | RunInPlace (RunFlags -> CommandDescr)
    -- ^ In this case the build return what you need to do the benchmark run, but the
    -- directory contents cannot be touched until after than run is finished.

-- | A completely encapsulated method of building benchmarks.  Cabal and Makefiles
-- are two examples of this.  The user may extend it with their own methods.
data BuildMethod =
  BuildMethod
  { methodName :: String          -- ^ Identifies this build method for humans.
--  , buildsFiles :: FilePredicate
--  , canBuild    :: FilePath -> IO Bool
  , canBuild    :: FilePredicate  -- ^ Can this method build a given file/directory?
  , concurrentBuild :: Bool -- ^ More than one build can happen at once.  This
                            -- implies that compile always returns StandAloneBinary.
  , compile :: CompileFlags -> FilePath -> IO BuildResult
  }

instance Show BuildMethod where
  show BuildMethod{methodName, canBuild} = "<buildMethod "++methodName++" "++show canBuild ++">"

----------------------------------------------------------------------------------------------------
-- HSBench Configuration
----------------------------------------------------------------------------------------------------

-- | A monad for benchamrking.  This provides access to configuration options, but
-- really, its main purpose is enabling logging.
type BenchM a = ReaderT Config IO a

-- | The global configuration for benchmarking:
data Config = Config 
 { benchlist      :: [Benchmark]
 , benchsetName   :: Maybe String -- ^ What identifies this set of benchmarks?  Used to create fusion table.
 , benchversion   :: (String, Double) -- ^ benchlist file name and version number (e.g. X.Y)
 , threadsettings :: [Int]  -- ^ A list of #threads to test.  0 signifies non-threaded mode.
 , maxthreads     :: Int
 , trials         :: Int    -- ^ number of runs of each configuration
 , shortrun       :: Bool
 , keepgoing      :: Bool   -- ^ keep going after error
 , ghc            :: String -- ^ ghc compiler path
 , cabalPath      :: String   
 , ghc_pkg        :: String
 , ghc_flags      :: String
 , ghc_RTS        :: String -- ^ +RTS flags
 , scheds         :: Set.Set Sched -- ^ subset of schedulers to test.
 , hostname       :: String
 , startTime      :: Integer -- ^ Seconds since Epoch. 
 , resultsFile    :: String -- ^ Where to put timing results.
 , logFile        :: String -- ^ Where to put more verbose testing output.

 , gitInfo        :: (String,String,Int)

 , buildMethods   :: [BuildMethod] -- ^ Starts with cabal/make/ghc, can be extended by user.
   
 -- These are all LINES-streams (implicit newlines).
 , logOut         :: Strm.OutputStream B.ByteString
 , resultsOut     :: Strm.OutputStream B.ByteString
 , stdOut         :: Strm.OutputStream B.ByteString
   -- A set of environment variable configurations to test
 , envs           :: [[(String, String)]]

 , doFusionUpload :: Bool
#ifdef FUSION_TABLES
 , fusionTableID  :: Maybe TableId -- ^ This must be Just whenever doFusionUpload is true.
 , fusionClientID :: Maybe String
 , fusionClientSecret :: Maybe String
--  , fusionUpload   :: Maybe FusionInfo
#endif
 }
 deriving Show

instance Show (Strm.OutputStream a) where
  show _ = "<OutputStream>"

----------------------------------------------------------------------------------------------------
-- Configuration Spaces
----------------------------------------------------------------------------------------------------

-- Represents a configuration of an individual run.
--  (number of
-- threads, other flags, etc):
data BenchRun = BenchRun
 { threads :: Int
 , sched   :: Sched 
 , bench   :: Benchmark
 , env     :: [(String, String)] -- ADDITIONAL bindings for the environment
 } deriving (Eq, Show, Ord)

data Benchmark = Benchmark
 { name :: String
 , compatScheds :: [Sched]
 , args :: [String]
 } deriving (Eq, Show, Ord)

-- TEMP: Remove this:
data Sched 
   = Trace | Direct | Sparks | ContFree | SMP | NUMA
   | None
 deriving (Eq, Show, Read, Ord, Enum, Bounded)


-- type BenchFile = [BenchStmt]

data Benchmark2 = Benchmark2
 { target  :: FilePath
 , cmdargs :: [String]
 , configs :: BenchSpace
 } deriving (Eq, Show, Ord)


-- | A datatype for describing (generating) benchmark configuration spaces.
--   This is accomplished by nested conjunctions and disjunctions.
--   For example, varying threads from 1-32 would be a 32-way Or.  Combining that
--   with profiling on/off (product) would create a 64-config space.
data BenchSpace = And [BenchSpace]
                | Or  [BenchSpace]
                | Set ParamSetting 
 deriving (Show,Eq,Ord,Read)

-- | Exhaustively compute all configurations described by a benchmark configuration space.
enumerateBenchSpace :: BenchSpace -> [ [ParamSetting] ] 
enumerateBenchSpace bs =
  case bs of
    Set p -> [ [p] ]
    Or ls -> concatMap enumerateBenchSpace ls
    And ls -> loop ls
  where
    loop [] = []
    loop [lst] = enumerateBenchSpace lst
    loop (hd:tl) =
      let confs = enumerateBenchSpace hd in
      [ c++r | c <- confs
             , r <- loop tl ]

test1 = Or (map (Set . RuntimeEnv "CILK_NPROCS" . show) [1..32])
test2 = Or$ map (Set . RuntimeParam "-A") ["1M", "2M"]
test3 = And [test1, test2]

-- | Different types of parameters that may be set or varied.
data ParamSetting 
  = RuntimeParam String String -- ^ These two strings are concattenated to make the option.
  | CompileParam String String -- ^ These two strings are concattenated to make the option.
  | RuntimeEnv   String String -- ^ The name of the env var and its value, respectively.
                               --   For now Env Vars ONLY affect runtime.
-- | Threads Int -- ^ Shorthand: builtin support for changing the number of
    -- threads across a number of separate build methods.
 deriving (Show, Eq, Read, Ord)

