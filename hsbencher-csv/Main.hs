{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import System.Environment (getArgs, getEnv, getEnvironment)
import System.Console.GetOpt (getOpt', ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.IO (Handle, hPutStrLn, stderr,stdin, openFile, hClose, hGetContents,
                  hIsEOF, hGetLine, IOMode(..), BufferMode(..), hSetBuffering, withFile)

import GHC.IO.Exception (IOException(..))

import Data.List (isInfixOf, intersperse, delete, transpose, sort,nub, deleteBy)

import Control.Monad (unless,when)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn)

import Data.Char (isNumber)

import qualified Data.Map as M 

-- list tweaking
import Data.List (elemIndex, intersperse, sortBy)


import Data.Maybe (catMaybes)

-- Exceptions
import Control.Exception
import Data.Typeable


-- cassava csv library - No Cassava does not do what we want.
-- cassava has this FromNamedRecord, ToNamedRecord requirements
-- if using csv with a header. these require that there is a conversion
-- to and from a record type of the csv entries (something you cannot do
-- if loading an arbitrary csv)


-- Trying Text.csv (CSV Loader and Dumper, does not use Bytestring though) 
import qualified Text.CSV as CSV
-- import Data.ByteString
-- import qualified Data.ByteString.Lazy.Char8 as BS
-- import Data.Vector hiding ((++))


import qualified Prelude as P
import Prelude hiding (init)
---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------

-- | Command line flags to the executable.
data Flag = ShowHelp | ShowVersion
          | File FilePath      
          | OutFile FilePath
            
-- Once things start to work more properly, exception handling should be made proper. 
-- | Exceptions that may occur
data Error
  = FlagsNotValidE String
    deriving (Show, Typeable) 

instance Exception Error 

-- | Command line options.
core_cli_options :: [OptDescr Flag]
core_cli_options = 
     [ Option ['h'] ["help"] (NoArg ShowHelp)
        "Show this help message and exit."
     , Option ['f'] ["file"] (ReqArg File "FileName.csv")    "Use CSV file as input"
     , Option ['o'] ["ouput"] (ReqArg OutFile "FileName.csv") "Name of result csv file" 
     ]

-- | Multiple lines of usage info help docs.
fullUsageInfo :: String
fullUsageInfo = usageInfo docs core_cli_options
 where 
  docs = "USAGE: grapher <flags> ...\n"++
         "\n\nhsbencher-graph general options: \n"
--   ++ generalUsageStr

---------------------------------------------------------------------------
-- CSV datatypes
---------------------------------------------------------------------------
data CSV = CSV CSV.Record  CSV.CSV
           deriving Show 
data CSVCol a = CSVCol [a] 
        deriving Show         
data CSVRow   = CSVRow [String]
                deriving Show 
data CSVValue a = CSVValue a
                deriving Show 
                  


---------------------------------------------------------------------------
-- MAIN
---------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  
  let (options,plainargs,_unrec,errs) = getOpt' Permute core_cli_options args

      inputSpecified  = (not . P.null) [() | File _ <- options]
      outputSpecified = (not . P.null) [() | OutFile _ <- options]

  --------------------------------------------------
  -- Stop running if no output
  when (not outputSpecified) $ error "No Output file specified" 

  --------------------------------------------------
  -- input or no input, keep going!
  let inFiles =
        case inputSpecified of
          False -> ["/dev/stdin"]
          True  -> [fp | File fp <- options] 

      -- testing
      file = head inFiles

  raw_data <- readFile file 
  
  let csv = extractCSV True raw_data -- CSV.parseCSV "" raw_data 

  putStrLn $ show csv 

---------------------------------------------------------------------------
-- extract CSV 
---------------------------------------------------------------------------

extractCSV :: Bool -> String -> CSV
extractCSV hasHeader raw_data =
  let csv = CSV.parseCSV "" raw_data

  in
   case csv of
     Left err -> error $ show err
     Right csv -> 
       case hasHeader of
         True -> if length csv >= 2
                 then CSV (head csv) (tail csv)
                 else error $ "Strange CSV file: \n" ++ show csv
         False -> CSV [] csv 
      


  
---------------------------------------------------------------------------
-- CSV coding language
---------------------------------------------------------------------------
--
--
--   API: 
--
--   Indexing and lookups:             
--   row :: Int -> CSV -> CSVRow 
--   Column :: ID -> CSV -> CSVCol a        -- a column should have same type along its extension
--   lookup :: Vector Key -> Vector Values -> CSVRow  -- The same can not be said about a row
--    
--   indexCol :: Int -> CSVCol a -> CSVValue a -- indexing into a column gives a CSVValue 
--   indexRow :: Key -> CSVRow -> CSVValue a   -- indexing into a row gives a CSVValue
--
--   Constructing CSVs
--   consCol :: Name -> CSVCol a -> CSV -> CSV    -- requires that a can be converted to bytestring (show?) 
--   consRow :: Header -> CSVRow -> CSV -> CSV
--   concatRight :: CSV -> CSV -> CSV
--   concatBelow :: CSV -> CSV -> CSV 
--
--   Column splitting:
--   SplitCol :: Key -> Char8 -> CSVCol ByteString -> vector (CSVCol ByteString) 
--   
--   Math:
--   (+) :: CSVValue Int -> CSVValue Int -> CSVValue Int
--   (-) :: CSVValue Int -> CSVValue Int -> CSVValue Int
--   (/)
--   (*)
--   ...
--   (+.) :: CSVValue Double -> CSVValue Double -> CSVValue Double
--   (-.) :: CSVValue Double -> CSVValue Double -> CSVValue Double
--   (/.)
--   (*.)
--   ... 
--
--   Collective operations
--   mapCSV :: (ByteString -> ByteString) -> CSV -> CSV 
--   mapCol :: (a -> b) -> CSVCol a -> CSVCol b
--   mapRow :: (ByteString -> ByteString) -> CSVRow -> CSVRow 
--
--
--   DataTypes: 
--   data CSV = CSV (Header, Vector (Vector ByteString))
--   data CSVCol a = CSVCol (Vector a)
--   data CSVRow   = CSVRow Header (Vector ByteString)
--   data CSVValue a = CSVValue a   -- a should be parseable from bytestring 








  
