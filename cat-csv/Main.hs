{-# LANGUAGE OverloadedStrings #-}

-- | Concatenate CSV files.

module Main where

import Control.Monad
import Data.List as L
import Data.Monoid
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Csv as C
-- import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.FilePath
import System.IO as IO

main :: IO ()
main = do
    args <- getArgs
    mapM_ check args
    when (null args) $ error "cat-csv: cannot concatenate zero CSV files."
    l@(hd:rst) <- mapM readCSV args
    let allColumns = nub $ concatMap fst l
        orderingInFirstFile = zip (fst hd) [0..]
        -- we add all extra fields in sorted order
        ordering = orderingInFirstFile ++
                    zip (allColumns \\ fst hd) [length orderingInFirstFile..]
        -- now we have orderings, convert all CSVs into dictionaries and
        -- print them in order
        dicts = concatMap (\(header, rows) ->
                  map (\row -> zip header row) rows) l

        -- Isn't this redundant?  Already in order?
        newHeader = map fst $ sortBy (\(_, i1) (_, i2) -> compare i1 i2) ordering

    hPutStrLn stderr $ "cat-csv: Final column ordering, len "++
                       show (length ordering)++": " ++show ordering

    let newRows = [ [ fromMaybe "" (lookup key dict) | key <- newHeader ]
                  | dict <- dicts ]
    BL.putStr $ C.encode (L.map V.fromList (newHeader:newRows))

--    T.putStrLn $ T.intercalate "," newHeader
{-
    forM_ dicts $ \ dict -> do
      -- hPutStrLn stderr $ "Formatting row of len" ++ show (length dict)
      let fields = [ fromMaybe "" (lookup key dict) | key <- newHeader ]
      T.putStrLn $ T.intercalate "," fields
-}
    {-
    let lines = T.unlines $ map (T.init . mconcat) $
          flip map dicts $ \dict ->
            flip map ordering $ \(h, _) ->
              case lookup h dict of
                Just v -> v <> ","
                Nothing -> ","
    T.putStrLn lines
    -}
  where
    check f = do
      b <- doesFileExist f
      unless b $ error $ "input file does not exist "++f



readCSV :: FilePath -> IO ([Text], [[Text]])
readCSV f = do bs <- BL.readFile f
               case C.decode C.NoHeader bs of
                Left err -> error $ "readCSV: "++show err
                Right dat -> do
                  let (hdr:rows) = V.toList dat
                  return (V.toList hdr, map V.toList rows)
