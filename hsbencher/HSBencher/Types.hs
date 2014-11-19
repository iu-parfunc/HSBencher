{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NamedFieldPuns, CPP  #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}


-- | All the core types used by the rest of the HSBencher codebase.

module HSBencher.Types
       (
         -- * Benchmark building
         -- | The basic types for describing a single benchmark.
         mkBenchmark, 
         Benchmark(..),          
         RunFlags, CompileFlags,

         -- * Build method interface and applicability
         -- | A build method is applicable to a subset of target files
         -- (`FilePredicate`) and has a particular interface that HSbencher relies
         -- upon.
         BuildMethod(..), BuildResult(..),         
         FilePredicate(..), filePredCheck,
         
         -- * Benchmark configuration spaces
         -- | Describe how many different ways you want to run your
         -- benchmarks.           
         BenchSpace(..), ParamSetting(..), CPUAffinity(..),
         enumerateBenchSpace, andAddParam, 
         compileOptsOnly, isCompileTime,
         toCompileFlags, toEnvVars, toCmdPaths,
         BuildID, makeBuildID,
         DefaultParamMeaning(..),
         
         -- * HSBencher Driver Configuration
         Config(..), BenchM, CleanupAction(..),

         -- * Subprocesses and system commands
         CommandDescr(..), RunResult(..), emptyRunResult,
         SubProcess(..), LineHarvester(..), orHarvest,

         -- * Benchmark outputs for upload
         BenchmarkResult(..), emptyBenchmarkResult, resultToTuple, tupleToResult,
--         Uploader(..), Plugin(..),
         SomePlugin(..), SomePluginConf(..), SomePluginFlag(..),

         Plugin(..), genericCmdOpts, getMyConf, setMyConf,

         SomeResult(..), Tag,
         
         -- * For convenience -- large records demand pretty-printing
         doc
       )
       where

import Control.Monad.Reader
import Data.Char
import Data.Word
import Data.List
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Dynamic
import Data.Default (Default(..))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (catMaybes)
import System.Console.GetOpt (getOpt, ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.FilePath
import System.Directory
import System.Process (CmdSpec(..))
import qualified Data.ByteString.Char8 as B
import qualified System.IO.Streams as Strm

import Text.PrettyPrint.GenericPretty (Out(doc,docPrec), Generic)

----------------------------------------------------------------------------------------------------
-- Benchmark Build Methods
----------------------------------------------------------------------------------------------------

type EnvVars      = [(String,String)]

-- | The arguments passed (in a build-method specific way) to the running benchmark.
type RunFlags     = [String]

-- | The arguments passed (in a build-method specific way) into the compilation process.
type CompileFlags = [String]

-- | Maps canonical command names, e.g. 'ghc', to absolute system paths.  This is a
--   global configuration mechanism that all BuildMethods have access to.
type PathRegistry = M.Map String String

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
  | AnyFile

  -- TODO: Allow arbitrary function predicates also.
 deriving (Show, Generic, Ord, Eq)
-- instance Show FilePredicate where
--   show (WithExtension s) = "<FilePredicate: *."++s++">"    


-- | This function gives meaning to the `FilePred` type.
--   It returns a filepath to signal "True" and Nothing otherwise.
filePredCheck :: FilePredicate -> FilePath -> IO (Maybe FilePath)
filePredCheck pred path =
  let filename = takeFileName path in 
  case pred of
    AnyFile           -> return (Just path)
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
      ls  <- getDirectoryContents (takeDirectory path)
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
  | RunInPlace (RunFlags -> EnvVars -> CommandDescr)
    -- ^ In this case the build return what you need to do the benchmark run, but the
    -- directory contents cannot be touched until after than run is finished.

instance Show BuildResult where
  show (StandAloneBinary p) = "StandAloneBinary "++p
--  show (RunInPlace fn)      = "RunInPlace "++show (fn [] [])
  show (RunInPlace fn)      = "RunInPlace <fn>"

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
  , compile :: Config -> BuildID -> CompileFlags -> FilePath -> BenchM BuildResult
              -- ^ Identify the benchmark to build by its target FilePath.  Compile it.
  , clean   :: PathRegistry -> BuildID -> FilePath -> BenchM () -- ^ Clean any left-over build results.
  , setThreads :: Maybe (Int -> [ParamSetting])
                  -- ^ Synthesize a list of compile/runtime settings that
                  -- will control the number of threads.
  }

instance Show BuildMethod where
  show BuildMethod{methodName, canBuild} = "<buildMethod "++methodName++" "++show canBuild ++">"

----------------------------------------------------------------------------------------------------
-- HSBench Configuration
----------------------------------------------------------------------------------------------------

-- | A monad for benchmarking  This provides access to configuration options, but
-- really, its main purpose is enabling logging.
type BenchM a = ReaderT Config IO a

-- | The global configuration for benchmarking.  WARNING! This is an internal data
-- structure.  You shouldn't really use it.
data Config = Config 
 { benchlist      :: [Benchmark DefaultParamMeaning]
 , extraParams    :: [ParamSetting] -- ^ Extra parameter settings to fold into EVERY benchmark we run.
 , benchsetName   :: Maybe String -- ^ What identifies this set of benchmarks?
                     -- In some upload backends this is the name of the dataset or table.
 , benchversion   :: (String, Double) -- ^ benchlist file name and version number (e.g. X.Y)
-- , threadsettings :: [Int]  -- ^ A list of #threads to test.  0 signifies non-threaded mode.
 , runTimeOut     :: Maybe Double -- ^ Timeout in seconds for running benchmarks 
                                  -- (if not specified by the benchmark specifically)
 , maxthreads     :: Int  -- ^ In parallel compile/run phases use at most this many threads.  
                          -- Defaults to `getNumProcessors`.
 , trials         :: Int  -- ^ number of runs of each configuration
 , skipTo         :: Maybe Int -- ^ Where to start in the config space.
 , runOnly        :: Maybe Int -- ^ How many configurations to run before stopping.
 , retryFailed    :: Maybe Int -- ^ How many times to retry failed benchmark configs.
 , runID          :: Maybe String -- ^ An over-ride for the run ID.
 , ciBuildID      :: Maybe String -- ^ The build ID from the continuous integration system.
 , shortrun       :: Bool  -- ^ An alternate mode to run very small sizes of benchmarks for testing.
                           --   HSBencher relies on a convention where benchmarks WITHOUT command-line
                           --   arguments must do a short run.
 , doClean        :: Bool  -- ^ Invoke the build method's clean operation before compilation.
 , doLSPCI        :: Bool  -- ^ Use the "lspci" command to gather more machine details on each run.
 , keepgoing      :: Bool  -- ^ Keep going after error.
 , pathRegistry   :: PathRegistry -- ^ Paths to executables
 , hostname       :: String  -- ^ Manually override the machine hostname.  
                             --   Defaults to the output of the `hostname` command.
 , defTopology    :: String -- ^ The default for the TOPOLOGY field, if a benchmark does not specify.
                            --   Usually, what cores we run on is fixed for a whole run of hsbencher.
 , startTime      :: Integer -- ^ Seconds since Epoch. 
 , resultsFile    :: String -- ^ Where to put timing results.
 , logFile        :: String -- ^ Where to put full, verbose testing output.

 , gitInfo        :: (String,String,Int) -- ^ Branch, revision hash, depth.

 , buildMethods   :: [BuildMethod] -- ^ Known methods for building benchmark targets.
                                   -- Starts with cabal/make/ghc, can be extended by user.
 , systemCleaner  :: CleanupAction
                    -- ^ An optional action to run between benchmark runs to make sure the system is clean.
                    -- For example, this could kill off zombie processes if any were left by previous
                    -- benchmark trials.
   
 -- These are all LINES-streams (implicit newlines).
 , logOut         :: Strm.OutputStream B.ByteString -- ^ Internal use only
 , resultsOut     :: Strm.OutputStream B.ByteString -- ^ Internal use only
 , stdOut         :: Strm.OutputStream B.ByteString -- ^ Internal use only
   -- A set of environment variable configurations to test
 , envs           :: [[(String, String)]]

 , argsBeforeFlags :: Bool -- ^ A global setting to control whether executables are given
                           -- their 'flags/params' after their regular arguments.
                           -- This is here because some executables don't use proper command line parsing.
 , harvesters      :: LineHarvester -- ^ A stack of line harvesters that gather RunResult details.

 , plugIns         :: [SomePlugin] -- ^ Each plugin, and, if configured, its configuration. 
 , plugInConfs     :: M.Map String SomePluginConf -- ^ Maps the `plugName` to its config.
 }
 deriving Show

-- ^ This is isomorphic to `Maybe (IO ())` but it has a `Show` instance.
data CleanupAction = NoCleanup
                   | Cleanup (IO ())
instance Show CleanupAction where
  show _ = "<CleanupAction>"

instance Show (Strm.OutputStream a) where
  show _ = "<OutputStream>"

----------------------------------------------------------------------------------------------------
-- Configuration Spaces
----------------------------------------------------------------------------------------------------

-- | The all-inclusive datatype for a single Benchmark.  Do NOT construct values of
-- this type directly.  Rather, you should make your code robust against future
-- addition of fields to this datatype.  Use `mkBenchmark` followed by customizing
-- only the fields you need.
data Benchmark a = Benchmark
 { target  :: FilePath  -- ^ Where is the benchmark to run?  This must be a single
                        -- target file or directory.  The convention is that this
                        -- file or directory, when combined with a BuildMethod,
                        -- provides a self-contained way to build one benchmark.  The
                        -- `buildMethods` field of the `Config` had better contain
                        -- some build method that knows how to handle this file or
                        -- directory.
 , cmdargs :: [String]      -- ^ Command line argument to feed the benchmark executable.
 , configs :: BenchSpace a  -- ^ The configration space to iterate over.
 , progname :: Maybe String -- ^ Optional name to use to identify this benchmark, INSTEAD of the basename from `target`.
 , benchTimeOut :: Maybe Double -- ^ Specific timeout for this benchmark in seconds.  Overrides global setting.
 , overrideMethod :: Maybe BuildMethod -- ^ Force use of this specific build method.
 } deriving (Show, Generic)

-- TODO: We could allow arbitrary shell scripts in lieu of the "target" file:
-- data BenchTarget
--   = PathTarget FilePath 
--   | ShellCmd String -- ^ A shell script to run the benchmark.  


-- | Make a Benchmark data structure given the core, required set of fields, and uses
-- defaults to fill in the rest.  Takes target, cmdargs, configs.
mkBenchmark :: FilePath -> [String] -> BenchSpace a -> Benchmark a 
mkBenchmark  target  cmdargs configs = 
  Benchmark {target, cmdargs, configs, progname=Nothing, benchTimeOut=Nothing, overrideMethod=Nothing }


-- | A datatype for describing (generating) benchmark configuration spaces.
--   This is accomplished by nested conjunctions and disjunctions.
--   For example, varying threads from 1-32 would be a 32-way Or.  Combining that
--   with profiling on/off (product) would create a 64-config space.
--
--   While the ParamSetting provides an *implementation* of the behavior, this
--   datatype can also be decorated with a (more easily machine readable) meaning of
--   the corresponding setting.  For example, indicating that the setting controls
--   the number of threads.
data BenchSpace meaning = And [BenchSpace meaning]
                        | Or  [BenchSpace meaning]
                        | Set meaning ParamSetting 
 deriving (Show,Eq,Ord,Read, Generic)

data DefaultParamMeaning
  = Threads Int    -- ^ Set the number of threads.
  | Variant String -- ^ Which scheduler/implementation/etc.
  | NoMeaning
 deriving (Show,Eq,Ord,Read, Generic)

-- | Exhaustively compute all configurations described by a benchmark configuration space.
enumerateBenchSpace :: BenchSpace a -> [ [(a,ParamSetting)] ] 
enumerateBenchSpace bs =
  case bs of
    Set m p -> [ [(m,p)] ]
    Or ls -> concatMap enumerateBenchSpace ls
    And ls -> loop ls
  where
    loop [] = [ [] ]  -- And [] => one config
    loop [lst] = enumerateBenchSpace lst
    loop (hd:tl) =
      let confs = enumerateBenchSpace hd in
      [ c++r | c <- confs
             , r <- loop tl ]


-- | Modify a config by `And`ing in an extra param setting to *every* `configs` field of *every*
-- benchmark in the global `Config`.
andAddParam :: ParamSetting -> Config -> Config
andAddParam param cfg =
  cfg { benchlist = map add (benchlist cfg) }
 where
   add bench@Benchmark{configs} =
     bench { configs = And [Set NoMeaning param, configs] }

-- TODO: We need to have a "filterBenchSpace" for filtration on the dynamic options. See issue #50 .

-- | Is it a setting that affects compile time?
isCompileTime :: ParamSetting -> Bool
isCompileTime CompileParam{} = True
isCompileTime CmdPath     {} = True
isCompileTime RuntimeParam{} = False
isCompileTime RuntimeArg{}   = False
isCompileTime RuntimeEnv  {} = False
isCompileTime CPUSet      {} = False

-- | Extract the parameters that affect the compile-time arguments.
toCompileFlags :: [(a,ParamSetting)] -> CompileFlags
toCompileFlags [] = []
toCompileFlags ((_,CompileParam s1) : tl) = s1 : toCompileFlags tl
toCompileFlags ((_,(RuntimeParam _)) : tl)      =  toCompileFlags tl
toCompileFlags ((_,(RuntimeArg _)) : tl)        =  toCompileFlags tl
toCompileFlags ((_,(RuntimeEnv _1 _2)) : tl)    =  toCompileFlags tl
toCompileFlags ((_,(CmdPath _1 _2))    : tl)    =  toCompileFlags tl
toCompileFlags ((_,(CPUSet _)) : tl)            =  toCompileFlags tl

toCmdPaths :: [(a,ParamSetting)] -> [(String,String)]
toCmdPaths = catMaybes . map fn
 where
   fn (_,CmdPath c p) = Just (c,p)
   fn _               = Nothing

toEnvVars :: [(a,ParamSetting)] -> [(String,String)]
toEnvVars [] = []
toEnvVars ((_,RuntimeEnv s1 s2)
           : tl) = (s1,s2) : toEnvVars tl
toEnvVars (_ : tl)                =           toEnvVars tl


-- | A BuildID should uniquely identify a particular (compile-time) configuration,
-- but consist only of characters that would be reasonable to put in a filename.
-- This is used to keep build results from colliding.
type BuildID = String

-- | Performs a simple reformatting (stripping disallowed characters) to create a
-- build ID corresponding to a set of compile flags.  To make it unique we also
-- append the target path.
makeBuildID :: FilePath -> CompileFlags -> BuildID
makeBuildID target strs =
  encodedTarget ++ 
  (intercalate "_" $
   map (filter charAllowed) strs)
 where
  charAllowed = isAlphaNum
  encodedTarget = map (\ c -> if charAllowed c then c else '_') target

-- | Strip all runtime options, leaving only compile-time options.  This is useful
--   for figuring out how many separate compiles need to happen.
compileOptsOnly :: BenchSpace a -> BenchSpace a 
compileOptsOnly x =
  case loop x of
    Nothing -> And []
    Just b  -> b
 where
   loop bs = 
     case bs of
       And ls -> mayb$ And$ catMaybes$ map loop ls
       Or  ls -> mayb$ Or $ catMaybes$ map loop ls
       Set m (CompileParam {}) -> Just bs
       Set m (CmdPath      {}) -> Just bs -- These affect compilation also...
       Set _ _                 -> Nothing
   mayb (And []) = Nothing
   mayb (Or  []) = Nothing
   mayb x        = Just x

test1 :: BenchSpace ()
test1 = Or (map (Set () . RuntimeEnv "CILK_NPROCS" . show) [1..32])

test2 :: BenchSpace ()
test2 = Or$ map (Set () . RuntimeParam . ("-A"++)) ["1M", "2M"]

test3 :: BenchSpace ()
test3 = And [test1, test2]

-- | Different types of parameters that may be set or varied.
data ParamSetting 
  = RuntimeParam String -- ^ String contains runtime options, expanded and tokenized by the shell.
  | RuntimeArg   String -- ^ Runtime "args" are like runtime params but are more prominent.  
                        --   They typically are part of the "key" of the benchmark.
  | CompileParam String -- ^ String contains compile-time options, expanded and tokenized by the shell.
--  CompileEnv   String String -- ^ Establish an environment variable binding during compile time.
  | RuntimeEnv   String String -- ^ The name of the env var and its value, respectively.
                               --   For now Env Vars ONLY affect runtime.
  | CmdPath      String String -- ^ Takes CMD PATH, and establishes a benchmark-private setting to use PATH for CMD.
                               --   For example `CmdPath "ghc" "ghc-7.6.3"`.
  | CPUSet  CPUAffinity -- ^ Set the cpu affinity in a particular way before launching the benchmark process.


-- | Threads Int -- ^ Shorthand: builtin support for changing the number of
    -- threads across a number of separate build methods.
-- | TimeOut      Double        -- ^ Set the timeout for this benchmark.
 deriving (Show, Eq, Read, Ord, Generic)

data CPUAffinity = Packed    -- ^ Picks cores packed into as few
                             -- NUMA domains as possible.
                 | SpreadOut -- ^ Picks cores spread over as many
                             -- NUMA domains as possible.
                 | Default
   deriving (Show, Eq, Read, Ord, Generic)

----------------------------------------------------------------------------------------------------
-- Subprocesses and system commands
----------------------------------------------------------------------------------------------------

-- | A self-contained description of a runnable command.  Similar to
-- System.Process.CreateProcess but slightly simpler.
data CommandDescr =
  CommandDescr
  { command :: CmdSpec            -- ^ Executable and arguments
  , envVars :: [(String, String)] -- ^ Environment variables to APPEND to current env.
  , timeout :: Maybe Double       -- ^ Optional timeout in seconds.
  , workingDir :: Maybe FilePath  -- ^ Optional working directory to switch to before
                                  --   running command.
  , tolerateError :: Bool         -- ^ Does a crash of the process mean we throw away any 
                                  --   data the program already printed?  Usually False.
  }
 deriving (Show,Eq,Ord,Read,Generic)

-- Umm... these should be defined in base:
deriving instance Eq   CmdSpec   
deriving instance Show CmdSpec
deriving instance Ord  CmdSpec
deriving instance Read CmdSpec   

-- | Measured results from running a subprocess (benchmark).
data RunResult =
    RunCompleted { realtime     :: Double       -- ^ Benchmark time in seconds, may be different than total process time.
                 , productivity :: Maybe Double -- ^ Seconds
                 , allocRate    :: Maybe Word64 -- ^ Bytes allocated per mutator-second
                 , memFootprint :: Maybe Word64 -- ^ High water mark of allocated memory, in bytes.
                 , jittime      :: Maybe Double -- ^ Time to JIT compile the benchmark, counted separately from realtime.
                    -- 
                 , custom       :: [(Tag,SomeResult)]
                 }
  | RunTimeOut
  | ExitError Int -- ^ Contains the returned error code.
 deriving (Eq,Show)

-- | A default `RunResult` that is a good starting point for filling in desired
-- fields.  (This way, one remains robust to additional fields that are added in the
-- future.)
emptyRunResult :: RunResult
emptyRunResult = RunCompleted { realtime = (-1.0)
                              , productivity = Nothing 
                              , allocRate = Nothing 
                              , memFootprint = Nothing
                              , jittime = Nothing
                              , custom = []}

-- | A running subprocess.
data SubProcess =
  SubProcess
  { wait :: IO RunResult
  , process_out  :: Strm.InputStream B.ByteString -- ^ A stream of lines.
  , process_err  :: Strm.InputStream B.ByteString -- ^ A stream of lines.
  }

instance Out CPUAffinity
instance Out ParamSetting
instance Out FilePredicate
instance Out DefaultParamMeaning
instance Out BuildMethod where
  docPrec n m = docPrec n $ methodName m
  doc         = docPrec 0
instance Out a => Out (BenchSpace a)
instance Out a => Out (Benchmark a)

instance (Out k, Out v) => Out (M.Map k v) where
  docPrec n m = docPrec n $ M.toList m
  doc         = docPrec 0 


-- | A line harvester takes a single line of input and possible extracts data from it
-- which it can then add to a RunResult.
-- 
-- The boolean result indicates whether the line was used or not.
newtype LineHarvester = LineHarvester (B.ByteString -> (RunResult -> RunResult, Bool))
-- newtype LineHarvester = LineHarvester (B.ByteString -> Maybe (RunResult -> RunResult))

-- | We can stack up line harvesters.  ALL of them get to run on each line.
instance Monoid LineHarvester where
  mempty = LineHarvester (\ _ -> (id,False))
  mappend (LineHarvester lh1) (LineHarvester lh2) = LineHarvester $ \ ln ->
    let (f,b1) = lh1 ln 
        (g,b2) = lh2 ln in
    (f . g, b1 || b2)

-- | Run the second harvester only if the first fails.
orHarvest :: LineHarvester -> LineHarvester -> LineHarvester
orHarvest (LineHarvester lh1) (LineHarvester lh2) = LineHarvester $ \ ln ->
  case lh1 ln of
    x@(_,True) -> x
    (_,False) -> lh2 ln 

instance Show LineHarvester where
  show _ = "<LineHarvester>"

----------------------------------------------------------------------------------------------------
-- Benchmark Results Upload
----------------------------------------------------------------------------------------------------
type Tag = String
data SomeResult = IntResult Int
                | DoubleResult Double 
                | StringResult String
                  -- expand here 
                deriving (Eq, Read, Ord)
instance Show SomeResult where
  show (IntResult i) = show i
  show (DoubleResult d) = show d
  show (StringResult str) = str 

-- | This contains all the contextual information for a single benchmark run, which
--   makes up a "row" in a table of benchmark results.
--   Note that multiple "trials" (actual executions) go into a single BenchmarkResult
data BenchmarkResult =
  -- FIXME: Threads should really be a (Maybe Nat):
  BenchmarkResult
  { _PROGNAME :: String    -- ^ Which benchmark are we running
  , _VARIANT  :: String    -- ^ If there are multiple ways to run the benchmark, this shoud record which was used.
  , _ARGS     :: [String]  -- ^ Command line arguments.
  , _HOSTNAME :: String    -- ^ Which machine did we run on?
  , _RUNID    :: String    -- ^ A unique identifier for the full hsbencher that included this benchmark.
  , _CI_BUILD_ID :: String -- ^ When launched from Jenkins or Travis, it can help to record where we came from.
  , _THREADS  :: Int       -- ^ If multithreaded, how many CPU threads did this benchmark run with, zero otherwise.       
  , _DATETIME :: String -- Datetime
  , _MINTIME    ::  Double -- ^ Time of the fastest run
  , _MEDIANTIME ::  Double -- ^ Time of the median run
  , _MAXTIME    ::  Double -- ^ Time of the slowest run
  , _MINTIME_PRODUCTIVITY    ::  Maybe Double  -- ^ GC productivity (if recorded) for the mintime run.
  , _MEDIANTIME_PRODUCTIVITY ::  Maybe Double  -- ^ GC productivity (if recorded) for the mediantime run.
  , _MAXTIME_PRODUCTIVITY    ::  Maybe Double  -- ^ GC productivity (if recorded) for the maxtime run.
  , _ALLTIMES      ::  String -- ^ Space separated list of numbers, should be one number for each TRIAL
  , _TRIALS        ::  Int    -- ^ How many times to [re]run each benchmark.
  , _COMPILER      :: String  
  , _COMPILE_FLAGS :: String  -- ^ Flags used during compilation
  , _RUNTIME_FLAGS :: String  -- ^ Flags passed at runtime, possibly in addition to ARGS
  , _ENV_VARS      :: String  -- ^ Environment variables set for this benchmark run
  , _BENCH_VERSION ::  String -- ^ If the benchmark *suite* tracks its version number, put it here.
  , _BENCH_FILE ::  String    
  , _UNAME      :: String     -- ^ Information about the host machine that ran the benchmark.
  , _PROCESSOR  :: String
  , _TOPOLOGY   :: String     -- ^ Some freeform indication of what cores we ran on
  , _GIT_BRANCH :: String     -- ^ Which branch was the benchmark run from
  , _GIT_HASH   :: String     -- ^ Which exact revision of the code was run.
  , _GIT_DEPTH  :: Int        -- ^ How many git commits deep was that rev (rough proxy for age)
  , _WHO        :: String     -- ^ Was anyone else logged into the machine?
  , _ETC_ISSUE  :: String     -- ^ Information about the host machine from /etc/issue
  , _LSPCI      :: String     -- ^ Information about the host machine from the lspci command
  , _FULL_LOG   :: String     -- ^ Optionally record the full stdout from the benchmarking process.
    
  , _MEDIANTIME_ALLOCRATE    ::  Maybe Word64  -- ^ If recorded, the allocation rate of the median run.
  , _MEDIANTIME_MEMFOOTPRINT ::  Maybe Word64  -- ^ If recorded, the memory footprint (high water mark) of the median run
  , _ALLJITTIMES   ::  String -- ^ Space separated list of numbers, JIT compile times
                              -- (if applicable), with a 1-1 correspondence to the exec times in ALLTIMES.
                              -- Time should not be double counted as JIT and exec time; these should be disjoint.
  , _RETRIES :: Int -- ^ The number of times any trial of the benchmark was reexecuted because of failure.
                        
  , _CUSTOM :: [(Tag, SomeResult)]
               -- ^ A List of custom results
               -- The tag corresponds to column "title"
  }
  deriving (Show,Read,Ord,Eq)

-- | A default value, useful for filling in only the fields that are relevant to a particular benchmark.
emptyBenchmarkResult :: BenchmarkResult
emptyBenchmarkResult = BenchmarkResult
  { _PROGNAME = ""
  , _VARIANT  = ""
  , _ARGS     = []
  , _HOSTNAME = ""
  , _RUNID    = ""
  , _CI_BUILD_ID = ""                
  , _THREADS  = 0
  , _DATETIME = "" 
  , _MINTIME    =  0.0
  , _MEDIANTIME =  0.0
  , _MAXTIME    =  0.0
  , _MINTIME_PRODUCTIVITY    =  Nothing
  , _MEDIANTIME_PRODUCTIVITY =  Nothing
  , _MAXTIME_PRODUCTIVITY    =  Nothing
  , _ALLTIMES      =  ""
  , _TRIALS        =  1
  , _COMPILER      = ""
  , _COMPILE_FLAGS = ""
  , _RUNTIME_FLAGS = ""
  , _ENV_VARS      = ""
  , _BENCH_VERSION =  ""
  , _BENCH_FILE =  ""
  , _UNAME      = ""
  , _PROCESSOR  = ""
  , _TOPOLOGY   = ""
  , _GIT_BRANCH = ""
  , _GIT_HASH   = ""
  , _GIT_DEPTH  = -1
  , _WHO        = ""
  , _ETC_ISSUE  = ""
  , _LSPCI      = ""
  , _FULL_LOG   = ""
  , _MEDIANTIME_ALLOCRATE    = Nothing
  , _MEDIANTIME_MEMFOOTPRINT = Nothing
  , _ALLJITTIMES = ""
  , _RETRIES = 0
  , _CUSTOM = [] 
  }

-- | Convert the Haskell representation of a benchmark result into a tuple for upload
-- to a typical database backend.
resultToTuple :: BenchmarkResult -> [(String,String)]
resultToTuple r =
  [ ("PROGNAME", _PROGNAME r)
  , ("VARIANT",  _VARIANT r)
  , ("ARGS",     unwords$ _ARGS r)    
  , ("HOSTNAME", _HOSTNAME r)
  , ("RUNID",    _RUNID r)
  , ("CI_BUILD_ID", _CI_BUILD_ID r)    
  , ("THREADS",  show$ _THREADS r)
  , ("DATETIME", _DATETIME r)
  , ("MINTIME",     show$ _MINTIME r)
  , ("MEDIANTIME",  show$ _MEDIANTIME r)
  , ("MAXTIME",     show$ _MAXTIME r)
  , ("MINTIME_PRODUCTIVITY",    fromMaybe "" $ fmap show $ _MINTIME_PRODUCTIVITY r)
  , ("MEDIANTIME_PRODUCTIVITY", fromMaybe "" $ fmap show $ _MEDIANTIME_PRODUCTIVITY r)
  , ("MAXTIME_PRODUCTIVITY",    fromMaybe "" $ fmap show $ _MAXTIME_PRODUCTIVITY r)
  , ("ALLTIMES",       _ALLTIMES r)
  , ("TRIALS",   show$ _TRIALS r)
  , ("COMPILER",       _COMPILER r)
  , ("COMPILE_FLAGS",  _COMPILE_FLAGS r)
  , ("RUNTIME_FLAGS",  _RUNTIME_FLAGS r)
  , ("ENV_VARS",       _ENV_VARS r)
  , ("BENCH_VERSION",  _BENCH_VERSION r)
  , ("BENCH_FILE",     _BENCH_FILE r)
  , ("UNAME",          _UNAME r)
  , ("PROCESSOR",      _PROCESSOR r)
  , ("TOPOLOGY",       _TOPOLOGY r)
  , ("GIT_BRANCH",     _GIT_BRANCH r)
  , ("GIT_HASH",       _GIT_HASH r)
  , ("GIT_DEPTH", show$ _GIT_DEPTH r)
  , ("WHO",            _WHO r)
  , ("ETC_ISSUE", _ETC_ISSUE r)
  , ("LSPCI", _LSPCI r)    
  , ("FULL_LOG", _FULL_LOG r)
  , ("MEDIANTIME_ALLOCRATE",    fromMaybe "" $ fmap show $ _MEDIANTIME_ALLOCRATE r)
  , ("MEDIANTIME_MEMFOOTPRINT", fromMaybe "" $ fmap show $ _MEDIANTIME_MEMFOOTPRINT r)    
  , ("ALLJITTIMES", _ALLJITTIMES r)
  , ("RETRIES", show (_RETRIES r))
  ] ++ map (\ (t,s) -> (t, show s)) (_CUSTOM r)

-- | Perform some validation and then convert raw CSV data to a BenchmarkResult.
tupleToResult :: [(String,String)] -> BenchmarkResult
tupleToResult tuple = BenchmarkResult
  { _PROGNAME = get "PROGNAME"
  , _VARIANT  = get "VARIANT" 
  , _ARGS     = words (get "ARGS")
  , _HOSTNAME = get "HOSTNAME"
  , _RUNID    = get "RUNID"
  , _CI_BUILD_ID = get "CI_BUILD_ID"
  , _THREADS  = s2i "THREADS" (try "THREADS" "0")
  , _DATETIME = get "DATETIME"
  , _MINTIME    =  s2d "MINTIME"    (try "MINTIME" "0.0")
  , _MEDIANTIME =  s2d "MEDIANTIME" (try "MEDIANTIME" "0.0")
  , _MAXTIME    =  s2d "MAXTIME"    (try "MAXTIME" "0.0")
  , _MINTIME_PRODUCTIVITY    = (tryMaybDbl "MINTIME_PRODUCTIVITY")
  , _MEDIANTIME_PRODUCTIVITY = (tryMaybDbl "MEDIANTIME_PRODUCTIVITY")
  , _MAXTIME_PRODUCTIVITY    = (tryMaybDbl "MAXTIME_PRODUCTIVITY")
  , _TRIALS        = s2i "TRIALS" (get "TRIALS")

  , _ALLTIMES      = get "ALLTIMES"
  , _COMPILER      = get "COMPILER"
  , _COMPILE_FLAGS = get "COMPILE_FLAGS"
  , _RUNTIME_FLAGS = get "RUNTIME_FLAGS"
  , _ENV_VARS      = get "ENV_VARS"
  , _BENCH_VERSION = get "BENCH_VERSION"
  , _BENCH_FILE = get "BENCH_FILE"
  , _UNAME      = get "UNAME"
  , _PROCESSOR  = get "PROCESSOR"
  , _TOPOLOGY   = get "TOPOLOGY"
  , _GIT_BRANCH = get "GIT_BRANCH"
  , _GIT_HASH   = get "GIT_HASH"
  , _GIT_DEPTH  = s2i "GIT_DEPTH" (get "GIT_DEPTH")
  , _WHO        = get "WHO"
  , _ETC_ISSUE  = get "ETC_ISSUE"
  , _LSPCI      = get "LSPCI"
  , _FULL_LOG   = get "FULL_LOG"
  , _ALLJITTIMES = get "ALLJITTIMES"
  , _RETRIES     = s2i "RETRIES" (get "RETRIES")
                     
  -- , _ALLTIMES      = try "ALLTIMES" ""
  -- , _COMPILER      = try "COMPILER" ""
  -- , _COMPILE_FLAGS = try "COMPILE_FLAGS" ""
  -- , _RUNTIME_FLAGS = try "RUNTIME_FLAGS" ""
  -- , _ENV_VARS      = try "ENV_VARS" ""
  -- , _BENCH_VERSION = try "BENCH_VERSION" ""
  -- , _BENCH_FILE = try "BENCH_FILE" ""
  -- , _UNAME      = try "UNAME" ""
  -- , _PROCESSOR  = try "PROCESSOR" ""
  -- , _TOPOLOGY   = try "TOPOLOGY" ""
  -- , _GIT_BRANCH = try "GIT_BRANCH" ""
  -- , _GIT_HASH   = try "GIT_HASH" ""
  -- , _GIT_DEPTH  = s2i (try "GIT_DEPTH" "-1")
  -- , _WHO        = try "WHO" ""
  -- , _ETC_ISSUE  = try "ETC_ISSUE" ""
  -- , _LSPCI      = try "LSPCI" ""
  -- , _FULL_LOG   = try "FULL_LOG" ""
  -- , _ALLJITTIMES = try "ALLJITTIMES" ""
  -- , _RETRIES = s2i (try "RETRIES" "0")
               
  , _MEDIANTIME_ALLOCRATE    = Nothing
  , _MEDIANTIME_MEMFOOTPRINT = Nothing
  , _CUSTOM = map parsecustom custom
  }
  where
    -- This is a hack where we simply see if it looks like a double or int:
    parsecustom (k,s) =
      case reads s of
        ((x,_):_)        -> (k, IntResult x)
        [] -> case reads s of
               ((x,_):_) -> (k, DoubleResult x)
               []        -> (k,StringResult s)
    
    custom = filter (\(k,_) -> not (S.member k schema)) tuple 
    
    schema = S.fromList (map fst (resultToTuple emptyBenchmarkResult))
    
    s2i :: String -> String -> Int
    s2i who s = case reads s of
                  [] -> error$ "tupleToResult: expected "++who++" field to contain a parsable Int, got: "++show s
                  ((x,_):_) -> x
    s2d :: String -> String -> Double
    s2d who s = case reads s of
                  [] -> error$ "tupleToResult: expected "++who++" field to contain a parsable Double, got: "++show s
                  ((x,_):_) -> x
    tupleM = M.fromList tuple
    tryMaybDbl key = case M.lookup key tupleM of
                       Nothing -> Nothing
                       Just "" -> Nothing
                       Just x  -> Just (s2d key x)
    
    try key df = case M.lookup key tupleM of
                   Nothing -> df
                   Just "" -> df
                   Just x  -> x
    get key = case M.lookup key tupleM of
                Nothing -> error$"tupleToResult: mandatory key missing from tuple: "++key
                Just x  -> x

--------------------------------------------------------------------------------
-- Generic uploader interface
--------------------------------------------------------------------------------

#if !MIN_VERSION_base(4,7,0)
instance Functor OptDescr where
  fmap fn (Option shrt long args str) = 
    Option shrt long (fmap fn args) str

instance Functor ArgDescr where
  fmap fn x = 
    case x of 
      NoArg x ->  NoArg (fn x)
      ReqArg fn2 str -> ReqArg (fn . fn2) str
      OptArg fn2 str -> OptArg (fn . fn2) str
#endif

--------------------------------------------------------------------------------

-- | An interface for plugins provided in separate packages.  These plugins provide
-- new backends for uploading benchmark data.
class (Show p, Eq p, Ord p,
       Show (PlugFlag p), Ord (PlugFlag p), Typeable (PlugFlag p), 
       Show (PlugConf p), Ord (PlugConf p), Typeable (PlugConf p),
       Default p, Default (PlugConf p)) => 
      Plugin p where
  -- | A configuration flag for the plugin (parsed from the command line)
  type PlugFlag p 
  -- | The full configuration record for the plugin.
  type PlugConf p 

  -- | Each plugin must have a unique name.
  plugName  :: p -> String

  -- | Options for command line parsing.  These should probably be disjoint from the
  --   options used by other plugins; so use very specific names.
  -- 
  --   Finally, note that the String returned here is a header line that is printed
  --   before the usage documentation when the benchmark executable is invoked with `-h`.
  plugCmdOpts :: p -> (String, [OptDescr (PlugFlag p)])

  -- | Process flags and update a configuration accordingly.
  foldFlags :: p -> [PlugFlag p] -> PlugConf p -> PlugConf p

  -- | Take any initialization actions, which may include reading or writing files
  -- and connecting to network services, as the main purpose of plugin is to provide
  -- backends for data upload.
  --
  -- Note that the initialization process can CHANGE the Config (it returns a new one).
  plugInitialize :: p -> Config -> IO Config

  -- | This is the raison d'etre for the class.  Upload a single row of benchmark data.
  plugUploadRow  :: p -> Config -> BenchmarkResult -> IO () 


data SomePlugin  = forall p . Plugin p => SomePlugin p 

-- | Keep a single flag together with the plugin it goes with.
data SomePluginFlag = 
--  forall p . (Plugin p, Typeable (PlugFlag p), Show (PlugFlag p)) => 
  forall p . (Plugin p) =>
  SomePluginFlag p (PlugFlag p)

-- | Keep a full plugin configuration together with the plugin it goes with.
data SomePluginConf = 
--  forall p . (Plugin p, Typeable (PlugConf p), Show (PlugConf p)) => 
  forall p . (Plugin p) =>
  SomePluginConf p (PlugConf p)

------------------------------------------------------------
-- Instances, very boring.

instance Show SomePlugin where
  show (SomePlugin p) = show p

instance Eq SomePlugin where 
  (SomePlugin p1) == (SomePlugin p2) = 
--    show p1 == show p2
    plugName p1 == plugName p2

instance Ord SomePlugin where 
  compare (SomePlugin p1) (SomePlugin p2) = 
    compare (plugName p1) (plugName p2)

instance Show SomePluginConf where
  show (SomePluginConf p pc) = show pc

instance Show SomePluginFlag where
  show (SomePluginFlag p f) = show f

------------------------------------------------------------

-- | Make the command line flags for a particular plugin generic so that they can be
-- mixed together with other plugins options.
genericCmdOpts :: Plugin p => p -> [OptDescr SomePluginFlag]
genericCmdOpts p = map (fmap lift) (snd (plugCmdOpts p))
 where 
 lift pf = SomePluginFlag p pf

-- | Retrieve our own Plugin's configuration from the global config.
--   This involves a dynamic type cast.
-- 
--   If there is no configuration for this plugin currently
--   registered, the default configuration for that plugin is
--   returned.
getMyConf :: forall p . Plugin p => p -> Config -> PlugConf p 
getMyConf p Config{plugInConfs} = 
  case M.lookup (plugName p) plugInConfs of 
--   Nothing -> error$ "getMyConf: expected to find plugin config for "++show p
   Nothing -> def :: (PlugConf p)
   Just (SomePluginConf p2 pc) -> 
     case (fromDynamic (toDyn pc)) :: Maybe (PlugConf p) of
       Nothing -> error $ "getMyConf: internal failure.  Performed lookup for plugin conf "
                          ++show p++" got back a conf for a different plugin " ++ show p2
       Just pc2 -> pc2

-- | Encapsulate the policy for where/how to inject the Plugin's conf into the global
-- Config.
setMyConf :: forall p . Plugin p => p -> PlugConf p -> Config -> Config 
setMyConf p new cfg@Config{plugInConfs} = 
  cfg { plugInConfs= (M.insert (plugName p) (SomePluginConf p new) plugInConfs) }

----------------------------------------
-- Small convenience functions

