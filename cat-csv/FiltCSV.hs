{-# LANGUAGE OverloadedStrings #-}

-- | Filter a subset of columns from a CSV file.
--
-- NOTE: This is subsumed by the `csvcut` command from the python `csvkit` library.

module Main where

-- import Control.Monad
-- import Data.List as L
-- import Data.Monoid
-- import Data.Maybe
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Csv as C
-- import qualified Data.Map as M
import qualified Data.Set as S
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as H
-- import System.Directory
import System.Environment
-- import System.FilePath
import System.IO as IO

main :: IO ()
main = do
    args <- getArgs
    let err = error "filt-csv expects one CSV file followed by a non-empty list of columns"
    case args of
      []  -> err
      [_] -> err
      (file:cols) ->
        do bstr <- BL.readFile file
           let Right (hdr,vec) = C.decodeByName bstr
               cols_bs = map B.pack cols
           hPutStrLn stderr $ "Parsed file with header: " ++ show hdr
           let filtered = V.map (prune (S.fromList cols_bs)) vec
           BL.putStr $ C.encodeByName (V.fromList  cols_bs)
                                      (V.toList filtered)


-- prune :: V.Vector C.Name ->  C.NamedRecord -> C.NamedRecord
prune :: S.Set B.ByteString ->  C.NamedRecord -> C.NamedRecord
prune cols record =
  H.filterWithKey (\k _v -> S.member k cols) record
