
-- | Support for custom tag harvesters.

module HSBencher.Harvesters ( -- * Custom tags have an expected type
                              customTagHarvesterInt,
                              customTagHarvesterDouble,
                              customTagHarvesterString,

                              -- * Accumulating tags record one sample from each TRIAL
                              customAccumHarvesterInt,
                              customAccumHarvesterDouble,
                              customAccumHarvesterString,

                              -- * Output Tags
                              fromTaggedLine, validTag
                              ) where

import           Data.ByteString.Char8 as B
import           Data.Char (isAlpha, isAlphaNum)
import qualified Data.List as L
import           HSBencher.Internal.MeasureProcess
import           HSBencher.Types

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
