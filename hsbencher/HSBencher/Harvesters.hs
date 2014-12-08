
module HSBencher.Harvesters ( customTagHarvesterInt,
                              customTagHarvesterDouble,
                              customTagHarvesterString) where

import HSBencher.Types
import HSBencher.Internal.MeasureProcess 

import Data.ByteString.Char8 as B

---------------------------------------------------------------------------
-- custom tag harvesters
--------------------------------------------------------------------------- 

customTagHarvesterInt :: String -> LineHarvester
customTagHarvesterInt tag =
  taggedLineHarvester (pack tag) $ 
    \d r -> r {custom = (tag,IntResult d) : custom r}


customTagHarvesterDouble :: String -> LineHarvester
customTagHarvesterDouble tag =
  taggedLineHarvester (pack tag) $
    \d r -> r {custom = (tag,DoubleResult d) : custom r}

customTagHarvesterString :: String -> LineHarvester
customTagHarvesterString tag =
  taggedLineHarvesterStr (pack tag) $
    \s r -> r {custom = (tag,StringResult s) : custom r}



---------------------------------------------------------------------------
-- Internal.
--------------------------------------------------------------------------- 
taggedLineHarvesterStr :: B.ByteString
                          -> (String -> RunResult -> RunResult)
                          -> LineHarvester
taggedLineHarvesterStr tag stickit = LineHarvester $ \ ln ->
  let failit = (id, False) in 
  case B.words ln of
    [] -> failit
    hd:tl | hd == tag || hd == (tag `B.append` (pack ":")) ->
      (stickit (unpack (B.unwords tl)), True)
    _ -> failit


-- Move other harvesters here? 
