#!/usr/bin/env runhaskell


-- This script: 
--   (1) crawls the current directory to find ALL results .dat files
--   (2) runs plot_scaling.hs on each
--   (3) follows up by gnuplot'ing and ps2pdfing all the resulting graphs

module Main where
import Data.Char
import Control.Monad
import HSH
import System.Directory
import System.FilePath 
import System.Environment
import Text.Printf

import ScriptHelpers

main = do 

  putStrLn "Usage ./plot_ALL [DIR]"
  putStrLn "  Searches for result_*.dat files and plots them."
  putStrLn "  * Default is to search ./*/*/results*.dat"
  putStrLn "  * Will use GHC environment variable"
  putStrLn "----------------------------------------"

  argv <- getArgs
  unless (null argv) $  putStrLn$ "Passing args on to plot_scaling script: "++ show argv

  env      <- getEnvironment
  let get v x = case lookup v env of 
		  Nothing -> x
		  Just  s -> s

  files <- case argv of
             []    -> run "ls */*/results_*.dat"
	     [dir] -> run ("find "++dir++" -name \"results_*.dat\" ")
  printf "Found %d results files:\n"  (length files)
  mapM_ (putStrLn . indent) files

  -- unlessM (doesFileExist "plot_scaling.exe") $ do 
  printf "\nFirst, build a binary version of the plotting script.\n"
  let ghc = get "GHC" "ghc"
      buildcmd = ghc++" --make plot_scaling.hs -o plot_scaling.exe"

  putStrLn$ indent$ buildcmd
  runIO$ buildcmd -|- indent

  printf "\nNext, plot each data file."

  forM_ files $ \ path -> do 

    let (dir, file) = splitFileName path
        summary     = replaceExtension path "summary"

    printf "\nProcessing %s:\n" file
    whenNewer path summary $ runIO$ ("./plot_scaling.exe "++path) -|- indent

    gps <- run ("find "++dir++" -name \"*.gp\" ")
    printf "\n  Generated %d gnuplot (.gp) files.  Building plots from them...\n" (length gps)
    forM_ (map trim gps) $ \ gp -> 
      whenNewer gp (replaceExtension gp "eps") $ do
        let (gpd,gpf) = splitFileName gp
        inDirectory gpd $ runEcho ("gnuplot " ++ gpf)

--       m1 <- getModificationTime gp
--       m2 <- getModificationTime eps
--       if m1 > m2 
--        then 
--        else printf "    %s up to date...\n" (takeFileName eps)

    printf "\n  Further, translating (%d) resulting .eps files to .pdf...\n" (length gps)
    epss <- run ("find "++dir++" -name \"*.eps\" ")
    forM_ (map trim epss) $ \ eps ->    
      whenNewer eps (replaceExtension eps "pdf") $ do
        let (epsd,epsf) = splitFileName eps
        inDirectory epsd $ runEcho ("ps2pdf "++ epsf)

--           pdf         = replaceExtension eps "pdf"
--       m1 <- getModificationTime eps
--       m2 <- getModificationTime pdf
--       if m1 > m2 
--        then inDirectory epsd $ runEcho ("ps2pdf "++ epsf)
--        else printf "    %s up to date...\n" (takeFileName pdf)
      

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


