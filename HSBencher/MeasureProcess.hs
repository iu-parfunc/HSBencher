{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

-- | This module provides tools to time a sub-process (benchmark), including a
-- facility for self-reporting execution time and reporting garbage collector
-- overhead.

module HSBencher.MeasureProcess
       (RunResult(..), SubProcess(..),
        measureProcess)
       where

import Data.IORef
import System.Exit
import System.Process (system, waitForProcess, getProcessExitCode, runInteractiveCommand, 
                       createProcess, CreateProcess(..), CmdSpec(..), StdStream(..), readProcess)
import qualified Control.Concurrent.Async as A

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified System.IO.Streams as Strm
import qualified System.IO.Streams.Concurrent as Strm
import qualified System.IO.Streams.Process as Strm
import qualified System.IO.Streams.Combinators as Strm
import qualified Data.ByteString.Char8 as B

--------------------------------------------------------------------------------

-- | Measured results from running a subprocess (benchmark).
data RunResult =
    RunCompleted { realtime     :: Double       -- ^ Benchmark time in seconds, may be different than total process time.
                 , productivity :: Maybe Double -- ^ Seconds
                 }
  | TimeOut
  | ExitError Int -- ^ Contains the returned error code.
 deriving (Eq,Show)

-- | A running subprocess.
data SubProcess =
  SubProcess
  { wait :: IO RunResult
  , process_out  :: Strm.InputStream B.ByteString -- ^ A stream of lines.
  , process_err  :: Strm.InputStream B.ByteString -- ^ A stream of lines.
  }
           
-- | This runs a sub-process and tries to determine how long it took (real time) and
-- how much of that time was spent in the mutator vs. the garbage collector.
--
-- It is complicated by:
--
--   (1) An additional protocol for the process to report self-measured realtime (a
--     line starting in "SELFTIMED")
--
--   (2) Parsing the output of "+RTS -s" to retrieve productivity OR using lines of
--       the form "PRODUCTIVITY: XYZ"
--
-- Note that "+RTS -s" is specific to Haskell/GHC, but the PRODUCTIVITY tag allows
-- non-haskell processes to report garbage collector overhead.
measureProcess ::
  String                -- ^ Executable
  -> [String]           -- ^ Command line arguments
  -> [(String, String)] -- ^ Environment variables
  -> Maybe Double       -- ^ Optional timeout in seconds.
  -> IO SubProcess
measureProcess cmd args env timeout = do
  startTime <- getCurrentTime
  (_inp,out,err,pid) <- Strm.runInteractiveProcess cmd args Nothing (Just env)
  out'  <- Strm.map OutLine =<< Strm.lines out
  err'  <- Strm.map ErrLine =<< Strm.lines err
  timeEvt <- case timeout of
               Nothing -> Strm.nullInput
               Just t  -> Strm.map (\_ -> TimerFire) =<< timeOutStream t

  -- Merging the streams is complicated because we want to catch when both stdout and
  -- stderr have closed (and not wait around until the timeout fires).
  ----------------------------------------
  merged0 <- Strm.concurrentMerge [out',err']
  merged1 <- reifyEOS merged0
  merged2 <- Strm.map (\x -> case x of
                              Nothing -> ProcessClosed
                              Just y -> y) merged1
  merged3 <- Strm.concurrentMerge [merged2, timeEvt]
  ----------------------------------------
  
  -- 'loop' below destructively consumes "merged" so we need fresh streams for output:
  relay_out <- newChan
  relay_err <- newChan  
  process_out <- Strm.chanToInput relay_out
  process_err <- Strm.chanToInput relay_err
  
  -- Process the input until there is no more, and then return the result.
  let 
      loop time prod = do
        x <- Strm.read merged3
        case x of
          Just ProcessClosed -> do
            writeChan relay_err Nothing
            writeChan relay_out Nothing
            code <- waitForProcess pid
            endtime <- getCurrentTime
            case code of
              ExitSuccess -> do
                tm <- case time of
                         -- If there's no self-reported time, we measure it ourselves:
                         Nothing -> let d = diffUTCTime endtime startTime in
                                    return$ fromRational$ toRational d
                         Just t -> return t
                return (RunCompleted {realtime=tm, productivity=prod})
              ExitFailure c   -> return (ExitError c)
        
          Just TimerFire -> return TimeOut
  
          -- Bounce the line back to anyone thats waiting:
          Just (ErrLine errLine) -> do 
            writeChan relay_err (Just errLine)
            -- Check for GHC-produced GC stats here:
            loop time (prod `orMaybe` checkProductivity errLine)
          Just (OutLine outLine) -> do
            writeChan relay_out (Just outLine)
            -- The SELFTIMED readout will be reported on stdout:
            loop (time `orMaybe` checkTimingLine outLine)
                 (prod `orMaybe` checkProductivity outLine)

          Nothing -> error "benchmark.hs: Internal error!  This should not happen."
  
  fut <- A.async (loop Nothing Nothing)
  return$ SubProcess {wait=A.wait fut, process_out, process_err}

-- | Internal data type.
data ProcessEvt = ErrLine B.ByteString
                | OutLine B.ByteString
                | ProcessClosed
                | TimerFire 
  deriving (Show,Eq,Read)

-------------------------------------------------------------------
-- Hacks for looking for particular bits of text in process output:
-------------------------------------------------------------------

-- | Check for a SELFTIMED line of output.
checkTimingLine :: B.ByteString -> Maybe Double
checkTimingLine ln =
  case B.words ln of
    [] -> Nothing
    hd:tl | hd == "SELFTIMED" || hd == "SELFTIMED:" ->
      case tl of
        [time] ->
          case reads (B.unpack time) of
            (dbl,_):_ -> Just dbl
            _ -> error$ "Error parsing number in SELFTIMED line: "++B.unpack ln
    _ -> Nothing

-- | Retrieve productivity (i.e. percent time NOT garbage collecting) as output from
-- a Haskell program with "+RTS -s".  Productivity is a percentage (double between
-- 0.0 and 100.0, inclusive).
checkProductivity :: B.ByteString -> Maybe Double
checkProductivity ln =
  case words (B.unpack ln) of
    [] -> Nothing
    -- EGAD: This is NOT really meant to be machine read:
    [p, time] | p == "PRODUCTIVITY" || p == "PRODUCTIVITY:" ->
       case reads time of
         (dbl,_):_ -> Just dbl
         _ -> error$ "Error parsing number in PRODUCTIVITY line: "++B.unpack ln
    ["GC","time",gc,"(",total,"elapsed)"] ->
--    "GC":"time": gc :"(": total :_ ->
        case (reads gc, reads total) of
          ((gcD,_):_,(totalD,_):_) -> Just $ 
            if totalD == 0.0
            then 100.0
            else (100 - (gcD / totalD * 100))
          _ -> error$ "checkGCTime: Error parsing number in MUT time line: "++B.unpack ln
   -- TODO: Support  "+RTS -t --machine-readable" as well...          
   --  read GC_wall_seconds     
   --  read mutator_wall_seconds
    _ -> Nothing


-- | Fire a single event after a time interval, then end the stream.
timeOutStream :: Double -> IO (Strm.InputStream ())
timeOutStream time = do
  s1 <- Strm.makeInputStream $ do
         threadDelay (round$ time * 1000 * 1000)
         return$ Just ()
  Strm.take 1 s1


orMaybe :: Maybe a -> Maybe a -> Maybe a 
orMaybe Nothing x  = x
orMaybe x@(Just _) _ = x 

-- | This makes the EOS into an /explicit/, penultimate message. This way it survives
-- `concurrentMerge`.  It represents this end of stream by Nothing, but beware the
-- doubly-nested `Maybe` type.
reifyEOS :: Strm.InputStream a -> IO (Strm.InputStream (Maybe a))
reifyEOS ins =
  do flag <- newIORef True
     Strm.makeInputStream $ do
       x   <- Strm.read ins
       flg <- readIORef flag
       case x of
         Just y -> return (Just (Just y))
         Nothing | flg -> do writeIORef flag False
                             return (Just Nothing)
                 | otherwise -> return Nothing

