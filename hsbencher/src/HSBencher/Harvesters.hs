{-# LANGUAGE OverloadedStrings #-}

-- | Support for Tag harvesters.  `Tags` are keywords like "SELFTIMED"
-- that HSBencher recognizes in benchmark output.  In particular,
-- HSBencher looks for lines of the form "TAG: VALUE" on stderr or stdout.

module HSBencher.Harvesters
       (
        -- * Built-in harvesters
        selftimedHarvester, jittimeHarvester,
        -- harvest_PROGNAME, harvest_VARIANT,
        allBuiltinHarvesters,

        -- * Non-standard GHC harvesters
        ghcProductivityHarvester, ghcAllocRateHarvester, ghcMemFootprintHarvester,

        -- * Utilities for constructing more harvesters
        taggedLineHarvester,

        -- * Custom tags, which have an expected type
        customTagHarvesterInt,
        customTagHarvesterDouble,
        customTagHarvesterString,

        -- * Accumulating tags record one sample from each TRIAL
        customAccumHarvesterInt,
        customAccumHarvesterDouble,
        customAccumHarvesterString,

        -- * Utilities for dealing with Tags
        fromTaggedLine, validTag
        ) where

import           Data.ByteString.Char8 as B
import           Data.Char (isAlpha, isAlphaNum, isSpace)
import qualified Data.List as L
-- import           HSBencher.Internal.MeasureProcess
import           HSBencher.Types
import           Prelude hiding (fail)

import Data.Monoid

-------------------------------------------------------------------
-- LineHarvesters: Hacks for looking for particular bits of text in process output:
-------------------------------------------------------------------

-- | Check for a SELFTIMED line of output.
selftimedHarvester :: LineHarvester
-- Ugly hack: line harvesters act on singleton trials, so MINTIME/MAXTIME make no sense.
-- But it's too convenient NOT to reuse the BenchmarkResult type here.
-- Thus we just set MEDIANTIME and ignore MINTIME/MAXTIME.
selftimedHarvester = taggedLineHarvester "SELFTIMED" (\d r -> r{ _MEDIANTIME = d
                                                               , _ALLTIMES = show d })

jittimeHarvester :: LineHarvester
jittimeHarvester = taggedLineHarvester "JITTIME" (\d r -> r{ _ALLJITTIMES = show (d::Double) })

-- TODO: Generate these somehow:

allBuiltinHarvesters :: LineHarvester
allBuiltinHarvesters =
  harvest_PROGNAME `mappend`
  harvest_VARIANT  `mappend`
  harvest_ARGS     `mappend`
  harvest_HOSTNAME `mappend`
  harvest_RUNID    `mappend`
  harvest_CI_BUILD_ID `mappend`
  harvest_THREADS     `mappend`
  harvest_DATETIME    `mappend`
  harvest_MINTIME     `mappend`
  harvest_MEDIANTIME  `mappend`
  harvest_MAXTIME     `mappend`
  harvest_MINTIME_PRODUCTIVITY    `mappend`
  harvest_MEDIANTIME_PRODUCTIVITY `mappend`
  harvest_MAXTIME_PRODUCTIVITY    `mappend`
  harvest_ALLTIMES      `mappend`
  harvest_TRIALS        `mappend`
  harvest_COMPILER      `mappend`
  harvest_COMPILE_FLAGS `mappend`
  harvest_RUNTIME_FLAGS `mappend`
  harvest_ENV_VARS      `mappend`
  harvest_BENCH_VERSION `mappend`
  harvest_BENCH_FILE   `mappend`
  harvest_UNAME        `mappend`
  harvest_PROCESSOR    `mappend`
  harvest_TOPOLOGY     `mappend`
  harvest_GIT_BRANCH   `mappend`
  harvest_GIT_HASH     `mappend`
  harvest_GIT_DEPTH    `mappend`
  harvest_WHO       `mappend`
  harvest_ETC_ISSUE `mappend`
  harvest_LSPCI     `mappend`
  harvest_FULL_LOG  `mappend`
  harvest_MEDIANTIME_ALLOCRATE     `mappend`
  harvest_MEDIANTIME_MEMFOOTPRINT  `mappend`
  harvest_ALLJITTIMES     `mappend`
  harvest_RETRIES


harvest_PROGNAME :: LineHarvester
harvest_PROGNAME = (taggedLineHarvester "PROGNAME" (\d r -> r{ _PROGNAME = d }))

harvest_VARIANT :: LineHarvester
harvest_VARIANT = (taggedLineHarvester "VARIANT" (\d r -> r{ _VARIANT = d }))

-- | Note, this harvester expects the ARGS to be written as a Haskell
-- datatype, compatible with the Read instance for `[String]`.
harvest_ARGS :: LineHarvester
harvest_ARGS = (taggedLineHarvester "ARGS" (\d r -> r{ _ARGS = d }))

harvest_HOSTNAME :: LineHarvester
harvest_HOSTNAME = (taggedLineHarvester "HOSTNAME" (\d r -> r{ _HOSTNAME = d }))

harvest_RUNID :: LineHarvester
harvest_RUNID = (taggedLineHarvester "RUNID" (\d r -> r{ _RUNID = d }))

harvest_CI_BUILD_ID :: LineHarvester
harvest_CI_BUILD_ID = (taggedLineHarvester "CI_BUILD_ID" (\d r -> r{ _CI_BUILD_ID = d }))

harvest_THREADS :: LineHarvester
harvest_THREADS = (taggedLineHarvester "THREADS" (\d r -> r{ _THREADS = d }))

harvest_DATETIME :: LineHarvester
harvest_DATETIME = (taggedLineHarvester "DATETIME" (\d r -> r{ _DATETIME = d }))

harvest_MINTIME :: LineHarvester
harvest_MINTIME = (taggedLineHarvester "MINTIME" (\d r -> r{ _MINTIME = d }))

harvest_MEDIANTIME :: LineHarvester
harvest_MEDIANTIME = (taggedLineHarvester "MEDIANTIME" (\d r -> r{ _MEDIANTIME = d }))

harvest_MAXTIME :: LineHarvester
harvest_MAXTIME = (taggedLineHarvester "MAXTIME" (\d r -> r{ _MAXTIME = d }))

harvest_MINTIME_PRODUCTIVITY :: LineHarvester
harvest_MINTIME_PRODUCTIVITY = (taggedLineHarvester "MINTIME_PRODUCTIVITY" (\d r -> r{ _MINTIME_PRODUCTIVITY = d }))

harvest_MEDIANTIME_PRODUCTIVITY :: LineHarvester
harvest_MEDIANTIME_PRODUCTIVITY = (taggedLineHarvester "MEDIANTIME_PRODUCTIVITY" (\d r -> r{ _MEDIANTIME_PRODUCTIVITY = d }))

harvest_MAXTIME_PRODUCTIVITY :: LineHarvester
harvest_MAXTIME_PRODUCTIVITY = (taggedLineHarvester "MAXTIME_PRODUCTIVITY" (\d r -> r{ _MAXTIME_PRODUCTIVITY = d }))

-- | ALLTIMES is read in as a single string of space separated numbers.
harvest_ALLTIMES :: LineHarvester
harvest_ALLTIMES = (taggedLineHarvester "ALLTIMES" (\d r -> r{ _ALLTIMES = d }))

harvest_TRIALS :: LineHarvester
harvest_TRIALS = (taggedLineHarvester "TRIALS" (\d r -> r{ _TRIALS = d }))

harvest_COMPILER :: LineHarvester
harvest_COMPILER = (taggedLineHarvester "COMPILER" (\d r -> r{ _COMPILER = d }))

harvest_COMPILE_FLAGS :: LineHarvester
harvest_COMPILE_FLAGS = (taggedLineHarvester "COMPILE_FLAGS" (\d r -> r{ _COMPILE_FLAGS = d }))

harvest_RUNTIME_FLAGS :: LineHarvester
harvest_RUNTIME_FLAGS = (taggedLineHarvester "RUNTIME_FLAGS" (\d r -> r{ _RUNTIME_FLAGS = d }))

harvest_ENV_VARS :: LineHarvester
harvest_ENV_VARS = (taggedLineHarvester "ENV_VARS" (\d r -> r{ _ENV_VARS = d }))

harvest_BENCH_VERSION :: LineHarvester
harvest_BENCH_VERSION = (taggedLineHarvester "BENCH_VERSION" (\d r -> r{ _BENCH_VERSION = d }))

harvest_BENCH_FILE :: LineHarvester
harvest_BENCH_FILE = (taggedLineHarvester "BENCH_FILE" (\d r -> r{ _BENCH_FILE = d }))

harvest_UNAME :: LineHarvester
harvest_UNAME = (taggedLineHarvester "UNAME" (\d r -> r{ _UNAME = d }))

harvest_PROCESSOR :: LineHarvester
harvest_PROCESSOR = (taggedLineHarvester "PROCESSOR" (\d r -> r{ _PROCESSOR = d }))

harvest_TOPOLOGY :: LineHarvester
harvest_TOPOLOGY = (taggedLineHarvester "TOPOLOGY" (\d r -> r{ _TOPOLOGY = d }))

harvest_GIT_BRANCH :: LineHarvester
harvest_GIT_BRANCH = (taggedLineHarvester "GIT_BRANCH" (\d r -> r{ _GIT_BRANCH = d }))

harvest_GIT_HASH :: LineHarvester
harvest_GIT_HASH = (taggedLineHarvester "GIT_HASH" (\d r -> r{ _GIT_HASH = d }))

harvest_GIT_DEPTH :: LineHarvester
harvest_GIT_DEPTH = (taggedLineHarvester "GIT_DEPTH" (\d r -> r{ _GIT_DEPTH = d }))

harvest_WHO :: LineHarvester
harvest_WHO = (taggedLineHarvester "WHO" (\d r -> r{ _WHO = d }))

harvest_ETC_ISSUE :: LineHarvester
harvest_ETC_ISSUE = (taggedLineHarvester "ETC_ISSUE" (\d r -> r{ _ETC_ISSUE = d }))

harvest_LSPCI :: LineHarvester
harvest_LSPCI = (taggedLineHarvester "LSPCI" (\d r -> r{ _LSPCI = d }))

harvest_FULL_LOG :: LineHarvester
harvest_FULL_LOG = (taggedLineHarvester "FULL_LOG" (\d r -> r{ _FULL_LOG = d }))

harvest_MEDIANTIME_ALLOCRATE :: LineHarvester
harvest_MEDIANTIME_ALLOCRATE = (taggedLineHarvester "MEDIANTIME_ALLOCRATE" (\d r -> r{ _MEDIANTIME_ALLOCRATE = d }))

harvest_MEDIANTIME_MEMFOOTPRINT :: LineHarvester
harvest_MEDIANTIME_MEMFOOTPRINT = (taggedLineHarvester "MEDIANTIME_MEMFOOTPRINT" (\d r -> r{ _MEDIANTIME_MEMFOOTPRINT = d }))

harvest_ALLJITTIMES :: LineHarvester
harvest_ALLJITTIMES = (taggedLineHarvester "ALLJITTIMES" (\d r -> r{ _ALLJITTIMES = d }))

harvest_RETRIES :: LineHarvester
harvest_RETRIES = (taggedLineHarvester "RETRIES" (\d r -> r{ _RETRIES = d }))

            --------------------------------------------------------------------------------

-- | Check for a line of output of the form "TAG: <VAL>".
--
--   Where <VAL> is a string matching the Haskell `Read` instance
--   for the corresponding type.
--
--   This only handles lines for which `fromTaggedLine` returns `Just`.
--
--  Lines without the specified tag are ignored.  Lines which have the
--  tag but have an unparseable value on the RHS result in an error.
--
taggedLineHarvester :: Read a => B.ByteString -> (a -> BenchmarkResult -> BenchmarkResult) -> LineHarvester
taggedLineHarvester tag stickit = LineHarvester $ \ ln ->
  let fail = (id, False) in
  case fromTaggedLine ln of
    Nothing -> fail
    -- Match either "TAG" or "TAG:"
    Just (tg,rhs) | tg == B.unpack tag ->
      case reads rhs of
        (val,_):_ -> (stickit val, True)
        _ -> error$ "[taggedLineHarvester] Error: line tagged with "
                  ++B.unpack tag++", but couldn't parse number: "++B.unpack ln
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
  (taggedLineHarvester "PRODUCTIVITY" (\d r -> r{ _MEDIANTIME_PRODUCTIVITY = Just d})) `orHarvest`  -- Otherwise we try to hack out the GHC "+RTS -s" output:
  (LineHarvester $ \ ln ->
   let nope = (id,False) in
   case L.words (B.unpack ln) of
     [] -> nope
     -- EGAD: This is NOT really meant to be machine read:
     ("Productivity": prod: "of": "total": "user," : _) ->
       case reads (L.filter (/= '%') prod) of
          ((prodN,_):_) -> (\r -> r{ _MEDIANTIME_PRODUCTIVITY = Just prodN }, True)
          _ -> nope
    -- TODO: Support  "+RTS -t --machine-readable" as well...
     _ -> nope)
ghcAllocRateHarvester :: LineHarvester
ghcAllocRateHarvester =
  (LineHarvester $ \ ln ->   let nope = (id,False) in
   case L.words (B.unpack ln) of
     [] -> nope
     -- EGAD: This is NOT really meant to be machine read:
     ("Alloc":"rate": rate: "bytes":"per":_) ->
       case reads (L.filter (/= ',') rate) of
          ((n,_):_) -> (\r -> r{ _MEDIANTIME_ALLOCRATE = Just n }, True)
          _ -> nope
     _ -> nope)

ghcMemFootprintHarvester :: LineHarvester
ghcMemFootprintHarvester =
  (LineHarvester $ \ ln ->
   let nope = (id,False) in
   case L.words (B.unpack ln) of
     [] -> nope
     -- EGAD: This is NOT really meant to be machine read:
--   "       5,372,024 bytes maximum residency (6 sample(s))",
     (sz:"bytes":"maximum":"residency":_) ->
       case reads (L.filter (/= ',') sz) of
          ((n,_):_) -> (\r -> r{ _MEDIANTIME_MEMFOOTPRINT = Just n}, True)
          _ -> nope
     _ -> nope)

---------------------------------------------------------------------------
-- custom tag harvesters
---------------------------------------------------------------------------

customTagHarvesterInt :: String -> LineHarvester
customTagHarvesterInt tag =
  taggedLineHarvester (pack tag) $
    \d r -> r { _CUSTOM = (tag,IntResult d) : _CUSTOM r}


customTagHarvesterDouble :: String -> LineHarvester
customTagHarvesterDouble tag =
  taggedLineHarvester (pack tag) $
    \d r -> r { _CUSTOM = (tag,DoubleResult d) : _CUSTOM r}

customTagHarvesterString :: String -> LineHarvester
customTagHarvesterString tag =
  taggedRawHarvester (pack tag) $
  \s r -> r { _CUSTOM = (tag,StringResult s) : _CUSTOM r }

-- Harvesters which accumulate their output over multiple "trials"
customAccumHarvesterInt :: String -> LineHarvester
customAccumHarvesterInt    = accumHarvester IntResult

customAccumHarvesterDouble :: String -> LineHarvester
customAccumHarvesterDouble = accumHarvester DoubleResult

customAccumHarvesterString :: String -> LineHarvester
customAccumHarvesterString = accumHarvester StringResult

accumHarvester :: Read a => (a -> SomeResult) -> String -> LineHarvester
accumHarvester ctr tag =
  taggedLineHarvester (pack tag) $ \s r ->
            r {
              -- RRN: Is this correct?  Should it instead see if there
              -- is already an entry at that tag and append to its list?
                _CUSTOM = (tag, AccumResult [ctr s]) : _CUSTOM r
              }

---------------------------------------------------------------------------
-- Internal.
---------------------------------------------------------------------------

-- TODO: Get rid of the use of String's in this section:

-- | Build a harvester that directly consumes the string on the
-- right-hand-side of the tagged line.  However, leading whitespace
-- that separates the "TAG:" from the value is stripped.  For example
-- the line "TAG: A B C" results in the string "A B C" being passed
-- along.
taggedRawHarvester :: B.ByteString
                      -> (String -> BenchmarkResult -> BenchmarkResult)
                      -> LineHarvester
taggedRawHarvester tag stickit = LineHarvester $ \ ln ->
  case fromTaggedLine ln of
    Nothing -> (id, False)
    Just (tg,val) | tg == unpack tag -> (stickit val, True)
                  | otherwise -> (id, False)

-- | Decompose a valid HSBencher tagged-line of output into its
-- constituent pieces, minus some whitespace.  This returns `Just` only if
-- the line starts with a valid tag followed by a colon.  Whitespace
-- between the tag and the colon is permitted but not encouraged.
--
-- Whitespace after the colon, before the value (right hand side), is
-- not required but may be in the future.  It is stripped in the RHS
-- returned by this function.
--
-- No assumptions about the tokenization of the RHS are made by this
-- function, and the RHS may even be empty.
--
-- The tag portion, on the other hand, MUST consist of exactly one
-- "word".
fromTaggedLine :: B.ByteString -> Maybe (Tag,String)
fromTaggedLine ln =
  -- case B.words ln of
  --   [] -> Nothing
  --   -- Whitespace between tag and colon:
  --   hd:col:tl | unpack col == ":" && validTag (unpack hd)
  --              -> Just (unpack hd, (unpack (B.unwords tl)))
  --   hd:tl | isColonTerminated (unpack hd) &&
  --           validTag (stripTag (unpack hd)) ->
  --     Just (stripTag (unpack hd), (unpack (B.unwords tl)))
  --   _ -> Nothing
  case B.uncons tl of
    Nothing -> Nothing -- No colon was found:
    Just (':', rhs) -> case B.words tag of
                         [tg] -> Just ( B.unpack tg,
                                        B.unpack $ B.dropWhile isSpace rhs)
                         _    -> Nothing
    Just _ -> Nothing -- Missing colon.
 where
  (hd,tl) = B.span (not . (==':')) ln
  tag = trim hd

trim :: ByteString -> ByteString
trim = B.dropWhile isSpace . B.reverse .
       B.dropWhile isSpace . B.reverse

-- | This defines HSBencher's concept of a valid tag for tagged output
-- on stdout or stderr.
validTag :: String -> Bool
validTag [] = False
validTag (a:rst) | isAlpha a = L.all isAlphaNum rst
                 | otherwise = False

-- | This is an internal utility for stripping the comma off the end.
_stripTag :: String -> String
_stripTag t = case L.reverse t of
                ':':ls -> L.reverse ls
                _ -> t

_isColonTerminated :: String -> Bool
_isColonTerminated t = case L.reverse t of
                         ':':_ -> True
                         _ -> False
