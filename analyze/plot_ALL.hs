#!/usr/bin/env runhaskell


-- This script: 
--   (1) crawls the desired directory to find ALL results .dat files
--   (2) runs plot_scaling.hs on each
--   (3) follows up by gnuplot'ing and ps2pdfing all the resulting graphs
--   (4) If successfull it removes all graphs but the pdfs to reduce clutter.

module Main where
import Data.Char
import Control.Monad
import HSH
import System.Directory (removeFile, getModificationTime, doesFileExist, )
import System.FilePath 
import System.Environment
import Text.Printf
import Control.Exception (handle, IOException)

import ScriptHelpers

--------------------------------------------------------------------------------

main = do 

  putStrLn "Usage ./plot_ALL [DIR]"
  putStrLn "  Searches for result_*.dat files and plots them."
  putStrLn "  * Default is to search ./*/*/*/*/results*.dat"
  putStrLn "  * Will use GHC environment variable"
  putStrLn "----------------------------------------"

  argv <- getArgs
  unless (null argv) $  putStrLn$ "Passing args on to plot_scaling script: "++ show argv

  env      <- getEnvironment
  let get v x = case lookup v env of 
		  Nothing -> x
		  Just  s -> s

  files <- case argv of
             -- Wacky default setting -- search at a certain depth for *.dat's:
             []    -> run "ls */*/*/*/results_*.dat"
	     [dir] -> do lns1 <- run ("find "++dir++" -type f -name \"results_*.dat\" ")
                         lns2 <- run ("find "++dir++" -type l -name \"results_*.dat\" ")
                         return (lns1 ++ lns2)
  printf "Found %d results files:\n"  (length files)
  mapM_ (putStrLn . indent) files

  printf "\nFirst, build a binary version of the plotting script.\n"
  let ghc = get "GHC" "ghc"
      buildcmd = ghc++" --make plot_scaling.hs -o plot_scaling.exe"
  putStrLn$ indent$ buildcmd
  runIO$ buildcmd -|- indent

  printf "\nNext, plot each data file."
  forM_ files $ \ path -> do 

--   catch (\ioerr -> putStrLn$ "ERROR - this plot attempt failed with: "++show ioerr)$ do
    handle (\e -> putStrLn$ "ERROR - this plot attempt failed with: "++show (e:: IOException))$ do
	  let (dir, file) = splitFileName path
	      summary     = replaceExtension path "summary"

	  printf "\n\nProcessing %s:\n" path
	  printf "====================================================================================================\n" 
	  whenNewer path summary $ runIO$ ("./plot_scaling.exe "++path) -|- indent

          -- Tweak:  Allow .gp / .dat / .eps files to be deleted.  Trigger the whole thing on the PDF being missing/older:
	  whenNewer summary (replaceExtension summary "pdf") $ do
	     gps <- run ("find "++dir++" -type f -name \"*.gp\" ")
	     printf "\n  Found %d gnuplot (.gp) files.  [maybe] building plots from them...\n" (length gps)
	     forM_ (map trim gps) $ \ gp -> 
	       whenNewer gp (replaceExtension gp "eps") $ do
		 let (gpd,gpf) = splitFileName gp
		 inDirectory gpd $ runEcho ("gnuplot " ++ gpf)

	     printf "\n  Further, found (%d) resulting .eps, possibly convert to .pdf...\n" (length gps)
	     epss <- run ("find "++dir++" -type f -name \"*.eps\" ")
	     forM_ (map trim epss) $ \ eps ->    
	       whenNewer eps (replaceExtension eps "pdf") $ do
		 let (epsd,epsf) = splitFileName eps
		     root = dropExtension epsf
		 inDirectory epsd $ runEcho ("ps2pdf "++ epsf)

		 -- Remove the extra files:
		 removeIfExists (root ++ ".eps")
		 removeIfExists (root ++ ".gp")
		 removeIfExists (root ++ ".dot")

    return ()


--------------------------------------------------------------------------------
-- Small helpers:


whenNewer f1 f2 action = do
    m1 <- getModificationTime f1
    e2 <- doesFileExist f2
    b <- if e2 then do
           m2 <- getModificationTime f2
	   return (m1 > m2)
         else return True
    if b 
     then action
     else printf "    %s is up-to-date...\n" (takeFileName f2)


removeIfExists file = do
  b <- doesFileExist file
  when b $ removeFile file
