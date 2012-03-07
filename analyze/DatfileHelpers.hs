{-# LANGUAGE NamedFieldPuns
  #-}

-- Dealing with .dat files.

module DatfileHelpers where 

import Text.PrettyPrint.HughesPJClass
import Debug.Trace (trace)

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
			 show (length orig) ++" expected 7 or 10:\n  "++ show (unwords orig)) $ 
		  Nothing
  where 
   defaultRec =
    Entry { name     = name, 
	    variant  = args,
	    sched    = sched,
	    threads  = read thrds,
	    tmin     = read t1,
	    tmed     = read t2,
	    tmax     = read t3,
	    productivities = Nothing,
	    normfactor = 1.0
	  }
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


