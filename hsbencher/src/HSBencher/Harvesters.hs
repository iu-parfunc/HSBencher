{-# LANGUAGE OverloadedStrings #-}

-- | Support for Tag harvesters.  `Tags` are keywords like "SELFTIMED"
-- that HSBencher recognizes in benchmark output.  In particular,
-- HSBencher looks for lines of the form "TAG: VALUE" on stderr or stdout.

module HSBencher.Harvesters
       (
        -- * Built-in harvesters
        selftimedHarvester, jittimeHarvester,
        harvest_PROGNAME,
        harvest_VARIANT,

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
import           Data.Char (isAlpha, isAlphaNum)
import qualified Data.List as L
-- import           HSBencher.Internal.MeasureProcess
import           HSBencher.Types
import           Prelude hiding (fail)

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

harvest_PROGNAME :: LineHarvester
harvest_PROGNAME = (taggedLineHarvester "PROGNAME" (\d r -> r{ _PROGNAME = d }))

harvest_VARIANT :: LineHarvester
harvest_VARIANT = (taggedLineHarvester "VARIANT" (\d r -> r{ _VARIANT = d }))


--------------------------------------------------------------------------------

-- | Check for a line of output of the form "TAG NUM" or "TAG: NUM".
--   Take a function that puts the result into place (the write half of a lens).
taggedLineHarvester :: Read a => B.ByteString -> (a -> BenchmarkResult -> BenchmarkResult) -> LineHarvester
taggedLineHarvester tag stickit = LineHarvester $ \ ln ->
  let fail = (id, False) in
  case B.words ln of
    [] -> fail
    -- Match either "TAG" or "TAG:"
    hd:tl | hd == tag || hd == (tag `B.append` ":") ->
      case tl of
        [time] ->
          case reads (B.unpack time) of
            (dbl,_):_ -> (stickit dbl, True)
            _ -> error$ "[taggedLineHarvester] Error: line tagged with "++B.unpack tag++", but couldn't parse number: "++B.unpack ln
        _ -> error$ "[taggedLineHarvester] Error: tagged line followed by more than one token: "++B.unpack ln
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
  taggedLineHarvesterStr (pack tag) $
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

taggedLineHarvesterStr :: B.ByteString
                          -> (String -> BenchmarkResult -> BenchmarkResult)
                          -> LineHarvester
taggedLineHarvesterStr tag stickit = LineHarvester $ \ ln ->
  case fromTaggedLine ln of
    Nothing -> (id, False)
    Just (tg,val) | tg == unpack tag -> (stickit val, True)
                  | otherwise -> (id, False)

-- | Decompose a valid HSBencher tagged-line of output into its
-- constituent pieces, whitespace stripped.  This returns Just only if
-- the line starts with a valid tag followed by a colon.  Whitespace
-- between the tag and the colon is permitted but not encouraged.
fromTaggedLine :: B.ByteString -> Maybe (Tag,String)
fromTaggedLine ln =
  case B.words ln of
    [] -> Nothing
    -- Whitespace between tag and colon:
    hd:col:tl | unpack col == ":" && validTag (unpack hd)
               -> Just (unpack hd, (unpack (B.unwords tl)))
    hd:tl | isColonTerminated (unpack hd) &&
            validTag (stripTag (unpack hd)) ->
      Just (stripTag (unpack hd), (unpack (B.unwords tl)))
    _ -> Nothing

-- | This defines HSBencher's concept of a valid tag for tagged output
-- on stdout or stderr.
validTag :: String -> Bool
validTag [] = False
validTag (a:rst) | isAlpha a = L.all isAlphaNum rst
                 | otherwise = False

-- | This is an internal utility for stripping the comma off the end.
stripTag :: String -> String
stripTag t = case L.reverse t of
               ':':ls -> L.reverse ls
               _ -> t

isColonTerminated :: String -> Bool
isColonTerminated t = case L.reverse t of
                        ':':_ -> True
                        _ -> False

-- Move other harvesters here?
