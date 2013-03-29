{-# LANGUAGE NamedFieldPuns
  #-}

-- Dealing with .dat files.

module DatfileHelpers 
--  readDataFile, remComments
where 

import Text.PrettyPrint.HughesPJClass
import Debug.Trace (trace)
import Data.Maybe (mapMaybe)
import Text.Regex
import Data.Char (isSpace, isDigit)

--------------------------------------------------------------------------------
-- Data Types:

-- | Here's the schema for one data-point from my timing tests:
data Entry = Entry { 
  name     :: String,
  variant  :: String,
  sched    :: String,
  threads  :: Int, 
  tmin     :: Double,
  tmed     :: Double,
  tmax     :: Double,
  -- These correspond to the tmin,tmed,tmax runs:
  productivities :: Maybe (Double,Double,Double),
  normfactor :: Double
}
  deriving Show

instance Pretty Entry where
  pPrint Entry { name, sched, variant, threads, tmin, tmed, tmax, normfactor } = 
       pPrint ("ENTRY", name, sched, variant, threads, (tmin, tmed, tmax), normfactor )

--------------------------------------------------------------------------------
-- Parsing 
--------------------------------------------------------------------------------

-- | Very sloppy parsing of individual lines of data from the text file.
parse :: [String] -> Maybe Entry
-- Example data line:
-- sorting/mergesort                   cpu_24_8192          SMP     28   4.459116 4.459116 4.459116   68.062 68.062 68.062
parse orig@(name:args:sched:thrds:t1:t2:t3:prods) = 
  case prods of
    []         -> Just$ defaultRec 
    [p1,p2,p3] -> Just$ defaultRec { productivities = Just (read p1, read p2, read p3) }
    ls         -> 
                  -- For now it's a warning rather an error if one data-point doesn't parse:
                  trace ("WARNING: Cannot parse, wrong number of fields, "++ 
                  show (length orig) ++" expected 7 or 10:\n  "++ show (unwords orig))
                  Nothing
  where 
   defaultRec =
    Entry { name     = name, 
	    variant  = args,
	    sched    = sched,
	    threads  = read thrds,
	    tmin     = read $ filterForDouble t1,
	    tmed     = read $ filterForDouble t2,
	    tmax     = read $ filterForDouble t3,
	    productivities = Nothing,
	    normfactor = 1.0
	  }
   filterForDouble = filter (\c -> isDigit c || c == '.')

-- Some older data entries look like this:
-- "queens Trace 4 3.71 3.76 3.78"
parse [name, sched, thrds, t1, t2, t3] = Just$ 
    Entry { name     = name, 
	    variant  = "_",
	    sched    = sched,
	    threads  = read thrds,
	    tmin     = read t1,
	    tmed     = read t2,
	    tmax     = read t3,
	    productivities = Nothing,
	    normfactor = 1.0
	  }
parse other = 
    trace ("WARNING: Cannot parse data line, too few fields: "++ show (unwords other)) $ 
    Nothing


parseDatFile :: String -> IO [Entry]
parseDatFile file = do 

-- dat <- run$ catFrom [file] -|- remComments 
 dat <- readDataFile file

 -- Here we filter bad or incomplete data points:
 let parsed0 = mapMaybe (parse . filter (not . (== "")) . splitRegex (mkRegex "[ \t]+")) 
	          (filter (not . isMatch (mkRegex "ERR")) $
		   filter (not . isMatch (mkRegex "TIMEOUT")) $
		   filter (not . null) dat)
 return parsed0

--------------------------------------------------------------------------------

-- | Remove comments from a list of lines.
-- Assumes hash is the comment character.
remComments :: [String] -> [String]
remComments ls = filter (pred . stripLeadingWS) ls
 where 
  commentChars = "#"
  pred str = not (take (length commentChars) str == commentChars) 
  stripLeadingWS []                = [] 
  stripLeadingWS (w:t) | isSpace w = stripLeadingWS t
  stripLeadingWS ls                = ls

-- Read our simple whitespace-separated data files:
-- Return the result as a list of word-lists.
-- readDataFile :: String -> IO [[String]]
readDataFile :: String -> IO [String]
readDataFile file = 
  do str <- readFile file
     return$ map unwords $ 
	     filter (not . null) $ 
             map words $
	     remComments $ lines str  


isMatch rg str = case matchRegex rg str of { Nothing -> False; _ -> True }
