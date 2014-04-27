{-# LANGUAGE NamedFieldPuns, OverloadedStrings, ScopedTypeVariables #-}

-- | This module provides tools to time a sub-process (benchmark), including a
-- facility for self-reporting execution time and reporting garbage collector
-- overhead for GHC-compiled programs.

module HSBencher.Internal.MeasureProcess
       (measureProcess,
        selftimedHarvester, jittimeHarvester,
        ghcProductivityHarvester, ghcAllocRateHarvester, ghcMemFootprintHarvester,
        taggedLineHarvester
        )
       where

import qualified Control.Concurrent.Async as A
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan
import qualified Control.Exception as E
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.IORef
import Data.Monoid
import System.Exit
import System.Directory
import System.IO (hClose, stderr)
import System.Process (system, waitForProcess, getProcessExitCode, runInteractiveCommand, terminateProcess, 
                       createProcess, CreateProcess(..), CmdSpec(..), StdStream(..), readProcess, ProcessHandle)
import System.Posix.Process (getProcessStatus)
import qualified System.IO.Streams as Strm
import qualified System.IO.Streams.Concurrent as Strm
import qualified System.IO.Streams.Process as Strm
import qualified System.IO.Streams.Combinators as Strm
import qualified Data.ByteString.Char8 as B
import System.Environment (getEnvironment)

import HSBencher.Types
import Debug.Trace

--------------------------------------------------------------------------------  

-- | This runs a sub-process and tries to determine how long it took (real time) and
-- how much of that time was spent in the mutator vs. the garbage collector.
--
-- It is complicated by:
--
--   (1) An additional protocol for the process to report self-measured realtime (a
--     line starting in "SELFTIMED", ditto for "JITTIME:")
--
--   (2) Parsing the output of GHC's "+RTS -s" to retrieve productivity OR using 
--       lines of the form "PRODUCTIVITY: XYZ"
--
-- Note that "+RTS -s" is specific to Haskell/GHC, but the PRODUCTIVITY tag allows
-- non-haskell processes to report garbage collector overhead.
--
-- This procedure is currently not threadsafe, because it changes the current working
-- directory.
measureProcess :: LineHarvester -- ^ Stack of harvesters
               -> CommandDescr
               -> IO SubProcess
measureProcess (LineHarvester harvest)
               CommandDescr{command, envVars, timeout, workingDir} = do
  origDir <- getCurrentDirectory
  case workingDir of
    Just d  -> setCurrentDirectory d
    Nothing -> return ()

  -- Semantics of provided environment is to APPEND:
  curEnv <- getEnvironment
  startTime <- getCurrentTime
  (_inp,out,err,pid) <-
    case command of
      RawCommand exeFile cmdArgs -> Strm.runInteractiveProcess exeFile cmdArgs Nothing (Just$ envVars++curEnv)
      ShellCommand str           -> runInteractiveCommandWithEnv str (envVars++curEnv)

  setCurrentDirectory origDir  -- Threadsafety!?!
  
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
      loop :: RunResult -> IO RunResult
      loop resultAcc = do
        x <- Strm.read merged3
        case x of
          Just ProcessClosed -> do
            writeChan relay_err Nothing
            writeChan relay_out Nothing
            code <- waitForProcess pid
            endtime <- getCurrentTime
            -- TODO: we should probably make this a Maybe type:
            if realtime resultAcc == realtime emptyRunResult
            then case code of
                   ExitSuccess -> 
                       -- If there's no self-reported time, we measure it ourselves:
                       let d = diffUTCTime endtime startTime in
                       return$ resultAcc { realtime = fromRational$ toRational d }
                   ExitFailure c   -> return (ExitError c)
            -- [2014.03.01] Change of policy.. if there was a SELFTIMED result, return it even if the process
            -- later errored.  (Accelerate/Cilk is segfaulting on exit right now.)
            else return resultAcc
          Just TimerFire -> do
            B.hPutStrLn stderr $ " [hsbencher] Benchmark run timed out.  Killing process."
            terminateProcess pid
            B.hPutStrLn stderr $ " [hsbencher] Cleaning up io-streams."
            writeChan relay_err Nothing
            writeChan relay_out Nothing
            E.catch (dumpRest merged3) $ \ (exn::E.SomeException) ->
              B.hPutStrLn stderr $ " [hsbencher] ! Got an error while cleaning up: " `B.append` B.pack(show exn)
            B.hPutStrLn stderr $ " [hsbencher] Done with cleanup."
            return RunTimeOut
  
          -- Bounce the line back to anyone thats waiting:
          Just (ErrLine errLine) -> do 
            writeChan relay_err (Just errLine)
            -- Check for GHC-produced GC stats here:
            loop $ fst (harvest errLine) resultAcc
          Just (OutLine outLine) -> do
            writeChan relay_out (Just outLine)
            -- The SELFTIMED readout will be reported on stdout:
            loop $ fst (harvest outLine) resultAcc

          Nothing -> error "benchmark.hs: Internal error!  This should not happen."
  
  fut <- A.async (loop emptyRunResult)
  return$ SubProcess {wait=A.wait fut, process_out, process_err}

-- Dump the rest of an IOStream until we reach the end
dumpRest :: Strm.InputStream a -> IO ()
dumpRest strm = do 
  x <- Strm.read strm
  case x of
    Nothing -> return ()
    Just _  -> dumpRest strm

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
selftimedHarvester :: LineHarvester
selftimedHarvester = taggedLineHarvester "SELFTIMED" (\d r -> r{realtime=d})

jittimeHarvester :: LineHarvester
jittimeHarvester = taggedLineHarvester "JITTIME" (\d r -> r{jittime=Just d})

-- | Check for a line of output of the form "TAG NUM" or "TAG: NUM".
--   Take a function that puts the result into place (the write half of a lens).
taggedLineHarvester :: B.ByteString -> (Double -> RunResult -> RunResult) -> LineHarvester
taggedLineHarvester tag stickit = LineHarvester $ \ ln ->
  let fail = (id, False) in 
  case B.words ln of
    [] -> fail
    hd:tl | hd == tag || hd == (tag `B.append` ":") ->
      case tl of
        [time] ->
          case reads (B.unpack time) of
            (dbl,_):_ -> (stickit dbl, True)
            _ -> error$ "Error: line tagged with "++B.unpack tag++", but couldn't parse number: "++B.unpack ln
    _ -> fail


--------------------------------------------------------------------------------
-- GHC-specific Harvesters:
--     
-- All three of these are currently using the human-readable "+RTS -s" output format.
-- We should switch them to "--machine-readable -s", but that would require combining
-- information harvested from multiple lines, because GHC breaks up the statistics.
-- (Which is actually kind of weird since its specifically a machine readable format.)

-- | Retrieve productivity (i.e. percent time NOT garbage collecting) as output from
-- a Haskell program with "+RTS -s".  Productivity is a percentage (double between
-- 0.0 and 100.0, inclusive).
ghcProductivityHarvester :: LineHarvester
ghcProductivityHarvester =
  -- This variant is our own manually produced productivity tag (like SELFTIMED):  
  (taggedLineHarvester "PRODUCTIVITY" (\d r -> r{productivity=Just d})) `orHarvest`
  (LineHarvester $ \ ln ->
   let nope = (id,False) in
   case words (B.unpack ln) of
     [] -> nope
     -- EGAD: This is NOT really meant to be machine read:
     ("Productivity": prod: "of": "total": "user," : _) ->
       case reads (filter (/= '%') prod) of
          ((prodN,_):_) -> (\r -> r{productivity=Just prodN}, True)
          _ -> nope
    -- TODO: Support  "+RTS -t --machine-readable" as well...          
     _ -> nope)

ghcAllocRateHarvester :: LineHarvester
ghcAllocRateHarvester =
  (LineHarvester $ \ ln ->
   let nope = (id,False) in
   case words (B.unpack ln) of
     [] -> nope
     -- EGAD: This is NOT really meant to be machine read:
     ("Alloc":"rate": rate: "bytes":"per":_) ->
       case reads (filter (/= ',') rate) of
          ((n,_):_) -> (\r -> r{allocRate=Just n}, True)
          _ -> nope
     _ -> nope)

ghcMemFootprintHarvester :: LineHarvester
ghcMemFootprintHarvester =
  (LineHarvester $ \ ln ->
   let nope = (id,False) in
   case words (B.unpack ln) of
     [] -> nope
     -- EGAD: This is NOT really meant to be machine read:
--   "       5,372,024 bytes maximum residency (6 sample(s))",
     (sz:"bytes":"maximum":"residency":_) ->
       case reads (filter (/= ',') sz) of
          ((n,_):_) -> (\r -> r{memFootprint=Just n}, True)
          _ -> nope
     _ -> nope)

--------------------------------------------------------------------------------

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

-- | Alternatioe to the io-streams version which does not allow setting the
-- environment.
runInteractiveCommandWithEnv :: String
                      -> [(String,String)]
                      -> IO (Strm.OutputStream B.ByteString,
                             Strm.InputStream  B.ByteString,
                             Strm.InputStream  B.ByteString,
                             ProcessHandle)
runInteractiveCommandWithEnv scmd env = do
    (Just hin, Just hout, Just herr, ph) <- createProcess 
       CreateProcess {
         cmdspec = ShellCommand scmd,
         env = Just env,
         std_in  = CreatePipe,
         std_out = CreatePipe,
         std_err = CreatePipe,
         cwd = Nothing,
         close_fds = False,
         create_group = False,
         delegate_ctlc = False
       }
    sIn  <- Strm.handleToOutputStream hin >>=
            Strm.atEndOfOutput (hClose hin) >>=
            Strm.lockingOutputStream
    sOut <- Strm.handleToInputStream hout >>=
            Strm.atEndOfInput (hClose hout) >>=
            Strm.lockingInputStream
    sErr <- Strm.handleToInputStream herr >>=
            Strm.atEndOfInput (hClose herr) >>=
            Strm.lockingInputStream
    return (sIn, sOut, sErr, ph)
