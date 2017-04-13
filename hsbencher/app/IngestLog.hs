{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simple application to parse logs.

module Main where

import           Control.Applicative
import           Control.Monad.Reader
import           Options.Applicative
import           System.FilePath
import           Data.Maybe
import           Data.Default 
import           Data.List as L
import           Data.Monoid
import           Data.List.Split (splitWhen)
-- import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BS
import           Prelude as P hiding (log)
import           Text.Show.Pretty (ppShow)

import           HSBencher.Harvesters
import           HSBencher.Backend.Dribble
import           HSBencher.Internal.Config
import           HSBencher.Internal.App
import           HSBencher.Internal.Logging (log)
import           HSBencher.Internal.Utils (fromLineOut)
import           HSBencher.Types

import           Paths_hsbencher (version)
import           Data.Version (showVersion)
--------------------------------------------------------------------------------    

data Opts = Opts { infile  :: Maybe FilePath
                 , outfile :: Maybe FilePath
                 }
 deriving (Eq,Show,Read,Ord)

argsParser :: Parser Opts
argsParser = Opts
             <$> optional (strArgument (metavar "InputFile.log"))
             <*> optional (strArgument (metavar "OutputFile.csv"))

-- https://github.com/iu-parfunc/HSBencher/issues/75

main :: IO ()
main = do
  let hdr = ("hsbencher-ingest-log: compiled with hsbencher version "++showVersion version++".")
  opts <- execParser $ info (helper <*> argsParser)
          (fullDesc
          <> progDesc
            (P.unlines
             [ "A utility to ingest logs and produce HSBencher-formatted CSV output,"
             , "as described in: https://github.com/iu-parfunc/HSBencher/issues/75"
             , "\n"
             , "Writes to the specified input/output files, if present, or from/to stdin/stdout otherwise."  
             ])          
          <> header hdr )
  ingestLog opts


-- TODO: Should do this all with constant space, conduit, etc...
ingestLog :: Opts -> IO ()
ingestLog opts@Opts{infile,outfile} = do
  putStrLn $ "Ingesting logs  "++show opts
  input <- case infile of
             Just f  -> BS.readFile f
             Nothing -> BS.getContents
  cfg0 <- getConfig [] []
  let cfg1 = addPlugin defaultDribblePlugin (DribbleConf { csvfile = outfile }) cfg0
  cfg2@Config{ harvesters } <- plugInitialize defaultDribblePlugin cfg1
           
  runReaderT (do

    -- Grab environmental information:
    starting <- augmentBenchmarkResult [] def

    -- ADD harvesters for PROGNAME, ARGS, VARIANT
    let harvs = -- taggedRawHarvester "PROGNAME" (\x r -> r{ _PROGNAME = x }) <>
                -- taggedRawHarvester "VARIANT"  (\x r -> r{ _VARIANT  = x }) <>
                -- taggedRawHarvester "THREADS"  (\x r -> r{ _THREADS  =
                --                                               trace ("SETTING THREADS "++x) $ 
                --                                               read x
                --                                         })
                -- taggedLineHarvester "ARGS"  (\x r -> r{ _ARGS     = words x }) <>
                harvesters

    let runRess = log2Runs input
  --  let results0 = parseRun def harvs (RunCompleted 0.0 (map OutLine (BS.lines input)))
    let results = map (parseOne starting harvs) runRess

--    lift $ putStrLn $ "Config: "++ppShow (cfg2)
    lift $ putStrLn $ "Extracted "++show (length runRess)++" individual benchmark results."
    lift $ putStrLn $ "Parsed "++show (length results)++" individual benchmark results."
--    lift $ mapM_ (putStrLn . ppShow) results
--    lift $ putStrLn $ "Threads: "++show (map _THREADS results)
         
--    final <- mapM (augmentBenchmarkResult []) results
    printBenchrunHeader
    log "Printing output to files..."
    mapM_ runC_outputBenchmarkResult results
    log "Done.")
   cfg2

-- | Parse the block of lines that corresponds to a single benchmark
-- run (including multiple trials)
parseOne :: BenchmarkResult -> LineHarvester -> OneRun -> BenchmarkResult
parseOne starting (LineHarvester fn) (OneRun lns) =
    accumed { _MEDIANTIME = median alltimes
            , _MINTIME    = minimum alltimes
            , _MAXTIME    = maximum alltimes
            , _ALLTIMES   = unwords (map show alltimes)
            }
 where
  accumed = L.foldl (\acc ln -> let (fn2,_) = fn ln
                                in fn2 acc)
                    starting lns
  alltimes = getSelfTimed lns


-- | Extract the SELFTIMED lines' values.
getSelfTimed :: [BS.ByteString] -> [Double]
getSelfTimed [] = []
getSelfTimed (x:xs) =
   case BS.words (x) of
     [] -> getSelfTimed xs
     [w1,w2] -> if BS.isPrefixOf "SELFTIMED" w1
                then case reads (BS.unpack w2) of
                       ((n,_):_) -> n : getSelfTimed xs
                       [] -> error$ "Could not parse RHS of SELFTIMED line: "++show x
                else getSelfTimed xs
     _ -> getSelfTimed xs

median :: (Fractional a, Ord a) => [a] -> a
median [] = error "cannot take the median value in an empty list!"
median ls =
  if even len
  then (ls !! half + ls!!(half-1) / 2)
  else ls !! half
 where
   len  = length ls
   half = len `quot` 2

-- | One run is a block of lines, some of which may be tags for us to process.
newtype OneRun = OneRun [BS.ByteString]
  deriving (Show,Eq,Ord,Read)
    
log2Runs :: BS.ByteString -> [OneRun]
log2Runs bs0 = [ OneRun lns | lns <- trimmed ]
  where
    allLines = BS.lines bs0
    chunks = case splitWhen isStart allLines of
               []   -> error "internal error"
               [[]] -> error "hsbencher-ingest-log: log did not contain START_BENCHMARK tag."
               (_trash:rest) -> rest -- Throw away the bit up to the FIRST START_BENCHMARK
    trimmed = [ takeWhile (not . isEnd) c | c <- chunks]



isStart :: BS.ByteString -> Bool
isStart b =
 case BS.words b of
   ("START_BENCHMARK":_)  -> True
   ("START_BENCHMARK:":_) -> True
   _                      -> False


isEnd :: BS.ByteString -> Bool
isEnd b =
 case BS.words b of
   ("END_BENCHMARK":_)  -> True
   ("END_BENCHMARK:":_) -> True
   _                    -> False
