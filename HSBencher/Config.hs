{-# LANGUAGE CPP, NamedFieldPuns, RecordWildCards #-}

-- | Code to deal with configuration information, including gathering it from the host environment.
--   Also deals with command line arguments.

-- Disabling some stuff until we can bring it back up after the big transition [2013.05.28]:
#define DISABLED

module HSBencher.Config
       ( -- * Configurations
         getConfig, augmentResultWithConfig,

         -- * Command line options
         Flag(..), all_cli_options
       )
       where

import Control.Monad.Reader
import qualified Data.Map as M
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Monoid
import GHC.Conc (getNumProcessors)
import System.Environment (getArgs, getEnv, getEnvironment)
import System.Console.GetOpt (getOpt, ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.IO (Handle, hPutStrLn, stderr, openFile, hClose, hGetContents, hIsEOF, hGetLine,
                  IOMode(..), BufferMode(..), hSetBuffering)
import qualified System.IO.Streams as Strm
import qualified System.IO.Streams.Concurrent as Strm
import qualified System.IO.Streams.Process as Strm
import qualified System.IO.Streams.Combinators as Strm


#ifdef FUSION_TABLES
import Network.Google.OAuth2 (getCachedTokens, refreshTokens, OAuth2Client(..), OAuth2Tokens(..))
import Network.Google.FusionTables (createTable, listTables, listColumns, insertRows,
                                    TableId, CellType(..), TableMetadata(..))
import HSBencher.Fusion (getTableId)
#endif


import HSBencher.Types
import HSBencher.Utils
import HSBencher.Methods
import HSBencher.MeasureProcess

----------------------------------------------------------------------------------------------------

-- | Command line flags.
data Flag = ParBench 
          | BinDir FilePath
          | NoRecomp | NoCabal | NoClean
          | ShortRun | KeepGoing | NumTrials String
          | SkipTo String | RunID String | CIBuildID String
          | CabalPath String | GHCPath String                               
          | ShowHelp | ShowVersion
#ifdef FUSION_TABLES
          | FusionTables (Maybe TableId)
          | BenchsetName (String)
          | ClientID     String
          | ClientSecret String
          | FusionTest
#endif
  deriving (Eq,Ord,Show,Read)

-- | Command line options.
core_cli_options :: (String, [OptDescr Flag])
core_cli_options = 
     ("\n Command Line Options:",
      [
#ifndef DISABLED        
        Option ['p'] ["par"] (NoArg ParBench) 
        "Build benchmarks in parallel (run in parallel too if SHORTRUN=1)."
#endif
        Option [] ["no-recomp"] (NoArg NoRecomp)
        "Don't perform any compilation of benchmark executables.  Implies -no-clean."
      , Option [] ["no-clean"] (NoArg NoClean)
        "Do not clean pre-existing executables before beginning."
      , Option [] ["shortrun"] (NoArg ShortRun)
        "Elide command line args to benchmarks to perform a testing rather than benchmarking run."
      , Option ['k'] ["keepgoing"] (NoArg KeepGoing)
        "Keep executing even after a build or run fails (default false)"
#ifndef DISABLED 
      , Option [] ["no-cabal"] (NoArg NoCabal)
        "A shortcut to remove Cabal from the BuildMethods"
#endif
      , Option [] ["with-cabal-install"] (ReqArg CabalPath "PATH")
        "Set the version of cabal-install to use for the cabal BuildMethod."
      , Option [] ["with-ghc"] (ReqArg GHCPath "PATH")
        "Set the path of the ghc compiler for the ghc BuildMethod."

      , Option [] ["trials"] (ReqArg NumTrials "NUM")
        "The number of times to run each benchmark."

      , Option [] ["runid"] (ReqArg RunID "NUM")
        "Force run ID to be a specific string; useful for completing failed runs"
      , Option [] ["buildid"] (ReqArg CIBuildID "STR")
        "Set the build ID used by the continuous integration system."

      , Option [] ["skipto"] (ReqArg (SkipTo ) "NUM")
        "Skip ahead to a specific point in the configuration space."

      , Option ['h'] ["help"] (NoArg ShowHelp)
        "Show this help message and exit."

      , Option ['V'] ["version"] (NoArg ShowVersion)
        "Show the version and exit"
     ])

all_cli_options :: [(String, [OptDescr Flag])]
all_cli_options = [core_cli_options]
#ifdef FUSION_TABLES
                ++ [fusion_cli_options]

fusion_cli_options :: (String, [OptDescr Flag])
fusion_cli_options =
  ("\n Fusion Table Options:",
      [ Option [] ["fusion-upload"] (OptArg FusionTables "TABLEID")
        "enable fusion table upload.  Optionally set TABLEID; otherwise create/discover it."

      , Option [] ["name"]         (ReqArg BenchsetName "NAME") "Name for created/discovered fusion table."
      , Option [] ["clientid"]     (ReqArg ClientID "ID")     "Use (and cache) Google client ID"
      , Option [] ["clientsecret"] (ReqArg ClientSecret "STR") "Use (and cache) Google client secret"
      , Option [] ["fusion-test"]  (NoArg FusionTest)   "Test authentication and list tables if possible." 
      ])
#endif


----------------------------------------------------------------------------------------------------

-- | Fill in "static" fields of a FusionTable row based on the `Config` data.
augmentResultWithConfig :: Config -> BenchmarkResult -> IO BenchmarkResult
augmentResultWithConfig Config{..} base = do
  -- ghcVer <- runSL$ ghc ++ " -V"
  -- let ghcVer' = collapsePrefix "The Glorious Glasgow Haskell Compilation System," "GHC" ghcVer
  datetime <- getCurrentTime
  uname    <- runSL "uname -a"
  lspci    <- runLines "lspci"
  whos     <- runLines "who"
  let newRunID = (hostname ++ "_" ++ show startTime)
  let (branch,revision,depth) = gitInfo      
  return $
    base
    { _HOSTNAME      = hostname
    , _RUNID         = case runID of
                        Just r -> r
                        Nothing -> newRunID
    , _CI_BUILD_ID   = case ciBuildID of
                        Just r -> r
                        Nothing -> ""
    , _DATETIME      = show datetime
    , _TRIALS        = trials
    , _ENV_VARS      = show envs 
    , _BENCH_VERSION = show$ snd benchversion
    , _BENCH_FILE    = fst benchversion
    , _UNAME         = uname
    , _LSPCI         = unlines lspci
    , _GIT_BRANCH    = branch   
    , _GIT_HASH      = revision 
    , _GIT_DEPTH     = depth
    , _WHO           = unlines whos
    }

-- Retrieve the (default) configuration from the environment, it may
-- subsequently be tinkered with.  This procedure should be idempotent.
getConfig :: [Flag] -> [Benchmark DefaultParamMeaning] -> IO Config
getConfig cmd_line_options benches = do
  hostname <- runSL$ "hostname -s"
  t0 <- getCurrentTime
  let startTime = round (utcTimeToPOSIXSeconds t0)
  env      <- getEnvironment

  -- There has got to be a simpler way!
  branch   <- runSL  "git name-rev --name-only HEAD"
  revision <- runSL  "git rev-parse HEAD"
  -- Note that this will NOT be newline-terminated:
  hashes   <- runLines "git log --pretty=format:'%H'"

  let       
      -- Read an ENV var with default:
      get v x = case lookup v env of 
		  Nothing -> x
		  Just  s -> s
      logFile = "bench_" ++ hostname ++ ".log"
      resultsFile = "results_" ++ hostname ++ ".dat"      

  case get "GENERIC" "" of 
    "" -> return ()
    s  -> error$ "GENERIC env variable not handled yet.  Set to: " ++ show s
  
  maxthreads <- getNumProcessors

  backupResults resultsFile logFile

  rhnd <- openFile resultsFile WriteMode 
  lhnd <- openFile logFile     WriteMode

  hSetBuffering rhnd NoBuffering
  hSetBuffering lhnd NoBuffering  
  
  resultsOut <- Strm.unlines =<< Strm.handleToOutputStream rhnd
  logOut     <- Strm.unlines =<< Strm.handleToOutputStream lhnd
  stdOut     <- Strm.unlines Strm.stdout

  let -- Messy way to extract the benchlist version:
      -- ver = case filter (isInfixOf "ersion") (lines benchstr) of 
      --         (h:_t) -> read $ (\ (h:_)->h) $ filter isNumber (words h)
      --         []    -> 0
      -- This is our starting point BEFORE processing command line flags:
      base_conf = Config 
           { hostname, startTime
           , shortrun       = False
           , doClean        = True
           , benchsetName   = Nothing
--	   , trials         = read$ get "TRIALS"    "1"
	   , trials         = 1
	   , skipTo         = Nothing
	   , runID          = Nothing
	   , ciBuildID      = Nothing                              
           , pathRegistry   = M.empty
--	   , benchlist      = parseBenchList benchstr
--	   , benchversion   = (benchF, ver)
           , benchlist      = benches
	   , benchversion   = ("",0)
	   , maxthreads     = maxthreads
--	   , threadsettings = parseIntList$ get "THREADS" (show maxthreads)
           , runTimeOut     = Just defaultTimeout
	   , keepgoing      = False
	   , resultsFile, logFile, logOut, resultsOut, stdOut         
--	   , outHandles     = Nothing
           , envs           = read $ get "ENVS" "[[]]"
           , gitInfo        = (trim branch, trim revision, length hashes)
           -- This is in priority order:                   
           , buildMethods   = [cabalMethod, makeMethod, ghcMethod]
           , doFusionUpload = False
           , argsBeforeFlags = True
           , harvesters = selftimedHarvester       `mappend`
                          ghcProductivityHarvester `mappend`
                          ghcMemFootprintHarvester `mappend`
                          ghcAllocRateHarvester    
#ifdef FUSION_TABLES
           , fusionConfig = FusionConfig 
              { fusionTableID  = Nothing 
              , fusionClientID     = lookup "HSBENCHER_GOOGLE_CLIENTID" env
              , fusionClientSecret = lookup "HSBENCHER_GOOGLE_CLIENTSECRET" env
              , serverColumns      = []
              }
#endif
	   }

  -- Process command line arguments to add extra cofiguration information:
  let 
#ifdef FUSION_TABLES
      doFlag (BenchsetName name) r     = r { benchsetName= Just name }
      doFlag (ClientID cid)   r = let r2 = fusionConfig r in
                                  r { fusionConfig= r2 { fusionClientID = Just cid } }
      doFlag (ClientSecret s) r = let r2 = fusionConfig r in
                                  r { fusionConfig= r2 { fusionClientSecret = Just s } }
      doFlag (FusionTables m) r = 
         let r2 = r { doFusionUpload = True } in
         case m of 
           Just tid -> let r3 = fusionConfig r in
                       r2 { fusionConfig= r3 { fusionTableID = Just tid } }
           Nothing -> r2
      doFlag FusionTest r = r
#endif
      doFlag (CabalPath p) r = r { pathRegistry= M.insert "cabal" p (pathRegistry r) }
      doFlag (GHCPath   p) r = r { pathRegistry= M.insert "ghc"   p (pathRegistry r) }

      doFlag ShortRun  r = r { shortrun= True }
      doFlag KeepGoing r = r { keepgoing= True }
      doFlag (NumTrials s) r = r { trials=
                                    case reads s of
                                      (n,_):_ -> n
                                      [] -> error$ "--trials given bad argument: "++s }
      doFlag (SkipTo s) r = r { skipTo=
                                    case reads s of
                                      (n,_):_ | n >= 1    -> Just n
                                              | otherwise -> error$ "--skipto must be positive: "++s
                                      [] -> error$ "--skipto given bad argument: "++s }
      doFlag (RunID s) r = r { runID= Just s }
      doFlag (CIBuildID s) r = r { ciBuildID= Just s }

      -- Ignored options:
      doFlag ShowHelp r = r
      doFlag ShowVersion r = r
      doFlag NoRecomp r = r
      doFlag NoCabal  r = r
      doFlag NoClean  r = r { doClean = False }
      doFlag ParBench r = r
      --------------------
      conf = foldr ($) base_conf (map doFlag cmd_line_options)

#ifdef FUSION_TABLES
  finalconf <- if not (doFusionUpload conf) then return conf else
               let fconf = fusionConfig conf in   
               case (benchsetName conf, fusionTableID fconf) of
                (Nothing,Nothing) -> error "No way to find which fusion table to use!  No name given and no explicit table ID."
                (_, Just tid) -> return conf
                (Just name,_) -> do
                  case (fusionClientID fconf, fusionClientSecret fconf) of
                    (Just cid, Just sec ) -> do
                      let auth = OAuth2Client { clientId=cid, clientSecret=sec }
                      (tid,cols) <- runReaderT (getTableId auth name) conf
                      return conf{ fusionConfig= fconf { fusionTableID= Just tid
                                                       , serverColumns= cols }}
                    (_,_) -> error "When --fusion-upload is activated --clientid and --clientsecret are required (or equiv ENV vars)"
#else
  let finalconf = conf      
#endif         
--  runReaderT (log$ "Read list of benchmarks/parameters from: "++benchF) finalconf
  return finalconf
