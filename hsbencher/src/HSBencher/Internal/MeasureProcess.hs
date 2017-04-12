{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

-- | This module provides tools to time a sub-process (benchmark), including a
-- facility for self-reporting execution time and reporting garbage collector
-- overhead for GHC-compiled programs.

module HSBencher.Internal.MeasureProcess
       ( -- measureProcess, measureProcessDBG,
        runSubprocessWithTimeOut, runSubprocess,

        -- * A utility for controlling CPU affinity
        setCPUAffinity
        )
       where

-- import qualified Control.Concurrent.Async as A
import Control.Concurrent (threadDelay)
-- import Control.Concurrent.Chan
-- import qualified Control.Exception as E
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.IORef
-- import Data.Default
import System.Exit
-- import System.Directory
import System.IO (hClose, stderr, hPutStrLn)
import System.Process (shell, system, waitForProcess)
import System.Process (createProcess, CreateProcess(..), CmdSpec(..), StdStream(..), readProcess, ProcessHandle)
import System.Posix.Process (getProcessID)
import qualified System.IO.Streams as Strm
-- import qualified System.IO.Streams.Concurrent as Strm
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import qualified Data.Set as S
import System.Environment (getEnvironment)

import HSBencher.Types
import Prelude hiding (fail)

--------------------------------------------------------------------------------


-- | This runs a sub-process, obeying the timout option specified in the CommandDescr.
--
--  This procedure does not parse the output of the program.  Rather,
--  it accumulates all process output in memory, waits until the
--  subprocess, and returns a RunResult containing the stdout/stderr
--  output plus the final status of the process.
--
--
--
--  This procedure is currently not threadsafe, because it changes the current working
--  directory.
runSubprocessWithTimeOut :: Maybe (Int,CPUAffinity)
                         -> CommandDescr
                         -> IO SubProcess
runSubprocessWithTimeOut = undefined



{-

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
-- directory.measureProcess :: Maybe (Int,CPUAffinity)
               -> LineHarvester -- ^ Stack of harvesters
               -> CommandDescr
               -> IO SubProcessmeasureProcess (Just aff) hrv descr =
  -- The way we do things here is we set it before launching the
  -- subprocess and we don't worry about unsetting it.  The child
  -- process will inherit the affinity.
  do setCPUAffinity aff
     measureProcess Nothing hrv descr

measureProcess Nothing (LineHarvester harvest)
               CommandDescr{command, envVars, timeout, workingDir, tolerateError} = do
  origDir <- getCurrentDirectory
  case workingDir of
    Just d  -> setCurrentDirectory d
    Nothing -> return ()

  -- Semantics of provided environment is to APPEND:
  curEnv <- getEnvironment
  startTime <- getCurrentTime
  (_inp,out,errout,pid) <-
    case command of
      RawCommand exeFile cmdArgs -> Strm.runInteractiveProcess exeFile cmdArgs Nothing (Just$ envVars++curEnv)
      ShellCommand str           -> runInteractiveCommandWithEnv str (envVars++curEnv)

  setCurrentDirectory origDir  -- Threadsafety!?!

  out'  <- Strm.map (Output . OutLine) =<< Strm.lines out
  err'  <- Strm.map (Output . ErrLine) =<< Strm.lines errout
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
            let retTime =
                   -- TODO: we should probably make this a Maybe type:
                   if realtime resultAcc == realtime def
                   then -- If there's no self-reported time, we measure it ourselves:
                        let d = diffUTCTime endtime startTime in
                        return$ resultAcc { realtime = fromRational$ toRational d }
                   else return resultAcc
            case code of
             ExitSuccess                   -> retTime
             ExitFailure c | tolerateError -> retTime
                           | otherwise     -> return (ExitError c)

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

          Nothing -> do
            let err = "benchmark.hs: Internal error!  This should not happen."
            B.hPutStrLn stderr err
            writeChan relay_err (Just err)
            error (B.unpack err)

  fut <- A.async (loop def)
  return$ SubProcess {wait=A.wait fut, process_out, process_err}

-}

-- | This is a simpler version of runSubprocessWithTimeOut that
-- ignores the timeout.  It is simpler because it is single threaded.
-- That is, it does not need to use additional Haskell IO threads to
-- implement the timeout behavior.
--
-- This variant is intended for debugging.  (It was originally part of
-- the process of trying to debug the HSBencher zombie state (Issue
-- #32).)
runSubprocess :: Maybe (Int,CPUAffinity)
              -> CommandDescr
              -> IO RunResult
runSubprocess (Just aff) descr =
  -- The way we do things here is we set it before launching the
  -- subprocess and we don't worry about unsetting it.  The child
  -- process will inherit the affinity.
  do setCPUAffinity aff
     runSubprocess Nothing descr

runSubprocess Nothing CommandDescr{command, envVars, timeout=_, workingDir, tolerateError} = do
  curEnv <- getEnvironment
  -- Create the subprocess:
  startTime <- getCurrentTime
  (Just _hin, Just hout, Just herr, ph) <- createProcess
     (shell "") {
       cmdspec = command,
       env = Just (envVars++curEnv),
       std_in  = CreatePipe,
       std_out = CreatePipe,
       std_err = CreatePipe,
       cwd = workingDir,
       close_fds = False,
       create_group = False,
       delegate_ctlc = False
     }
    -- TODO: implement timeout!
    -- TODO: Could sleep and flush the buffer inbetween sleeping so as
    -- to avoid forking extra threads here.

  -- Read stdout till it closes:
  out <- B.hGetContents hout
  err <- B.hGetContents herr
  code <- waitForProcess ph
  endtime <- getCurrentTime
  let outl, errl :: [B.ByteString]
      outl = B.lines out
      errl = B.lines err
      tagged =  map OutLine outl ++ map ErrLine errl
--      result = foldr (fst . harvest) def (outl++errl)

  time <-
   -- if _MEDIANTIME result /= _MEDIANTIME def  -- Is it set to anything?
   -- then return (_MEDIANTIME result)
   -- else -- If there's no self-reported time, we measure it ourselves:
        let d = diffUTCTime endtime startTime in
        return (fromRational$ toRational d)
  return $ case code of
            ExitSuccess                   -> RunCompleted time tagged
            -- Here we tolerate errors but we do NOT use process time:
            ExitFailure c | tolerateError -> RunCompleted (-1.0) tagged
                          | otherwise     -> ExitError c tagged


-- Dump the rest of an IOStream until we reach the end
_dumpRest :: Strm.InputStream a -> IO ()
_dumpRest strm = do
  x <- Strm.read strm
  case x of
    Nothing -> return ()
    Just _  -> _dumpRest strm

-- | Internal data type.
data ProcessEvt = Output LineOut
                | ProcessClosed
                | TimerFire
  deriving (Show,Eq,Read)

-------------------------------------------------------------------
-- Affinity
-------------------------------------------------------------------

-- | Set the affinity of the *current* process.
setCPUAffinity :: (Int,CPUAffinity) -> IO ()
setCPUAffinity (numthreads,aff) = do
  pid <- getProcessID
  -- We use readProcess to error out if there is a non-zero exit code:
  dump <- readProcess "numactl" ["--hardware"] []
  let filt0 = filter (L.isInfixOf "cpus:") (lines dump)
      cpusets = [ rst | ("node":_:"cpus:":rst) <- map words filt0 ]
      numDomains = length cpusets
      allCPUs = concat cpusets
  hPutStrLn stderr $ " [hsbencher] numactl found  "++show numDomains++
            " NUMA domains: "++show (map (map readInt) cpusets)
--  __fin  "numactl --hardware | grep cpus:"
  let assertLen n l
        | length l == n = l
        | otherwise = error $ "setCPUAffinity: requested "++show n
                             ++" cpus, only got "++show (length l)
      subset = case aff of
                Default  -> concat cpusets
                Packed   -> assertLen numthreads $ take numthreads (concat cpusets)
                SpreadOut -> assertLen numthreads $
                  let (q,r) = numthreads `quotRem` numDomains
                      frsts = concat $ map (take q) cpusets
                      leftover = S.toList (S.difference (S.fromList allCPUs) (S.fromList frsts))
                  in frsts ++ take r leftover

  case subset of
    [] -> error "setCPUAffinity: internal error: zero cpus selected."
    _  -> do
      let cmd = ("taskset -pc "++L.intercalate "," subset++" "++show pid)
      hPutStrLn stderr $ " [hsbencher] Attempting to set CPU affinity: "++cmd
      cde <- system cmd
      -- TODO: Having problems with hyperthreading.  Read back the
      -- affinity set and make sure it MATCHES!
      case cde of
        ExitSuccess -> return ()
        ExitFailure c -> error $ "setCPUAffinity: taskset command returned error code: "++show c

-- TODO: parse strings like 0-3,10 or parse the hex representation
-- parse taskset

readInt :: String -> Int
readInt = read


--------------------------------------------------------------------------------

-- | Fire a single event after a time interval, then end the stream.
_timeOutStream :: Double -> IO (Strm.InputStream ())
_timeOutStream time = do
  s1 <- Strm.makeInputStream $ do
         threadDelay (round$ time * 1000 * 1000)
         return$ Just ()
  Strm.take 1 s1


-- | This makes the EOS into an /explicit/, penultimate message. This way it survives
-- `concurrentMerge`.  It represents this end of stream by Nothing, but beware the
-- doubly-nested `Maybe` type.
_reifyEOS :: Strm.InputStream a -> IO (Strm.InputStream (Maybe a))
_reifyEOS ins =
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
_runInteractiveCommandWithEnv :: String
                      -> [(String,String)]
                      -> IO (Strm.OutputStream B.ByteString,
                             Strm.InputStream  B.ByteString,
                             Strm.InputStream  B.ByteString,
                             ProcessHandle)
_runInteractiveCommandWithEnv scmd env = do
    (Just hin, Just hout, Just herr, ph) <- createProcess
       (shell scmd) {
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
