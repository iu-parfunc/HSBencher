{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE NamedFieldPuns      #-}
-- |
-- Seeded from code by:
-- Copyright    : [2014] Trevor L. McDonell

module Main where

-- Friends:
import HSBencher
import HSBencher.Internal.Config (augmentResultWithConfig, getConfig)
import HSBencher.Backend.Fusion

-- Standard:
import Control.Monad.Reader
import Data.List as L
import System.Console.GetOpt (getOpt, getOpt', ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.Environment (getArgs)
import System.Exit
import qualified Data.Map                               as Map

import Text.CSV

this_progname = "hsbencher-fusion-upload-csv"

----------------------------------------------------------------------------------------------------

data ExtraFlag = TableName String
               | PrintHelp
  deriving (Eq,Ord,Show,Read)

extra_cli_options :: [OptDescr ExtraFlag]
extra_cli_options =  [ Option ['h'] ["help"] (NoArg PrintHelp)
                       "Show this help message and exit."
                     , Option [] ["name"] (ReqArg TableName "NAME")
                       "Name for the fusion table to which we upload (discovered or created)." ]
plug :: FusionPlug
plug = defaultFusionPlugin

main :: IO ()
main = do
   cli_args <- getArgs
   let (help,fusion_cli_options) = plugCmdOpts plug

   let (opts1,plainargs,unrec,errs1) = getOpt' Permute extra_cli_options cli_args
   let (opts2,_,errs2) = getOpt Permute fusion_cli_options unrec
   let errs = errs1 ++ errs2   
   when (L.elem PrintHelp opts1 || not (null errs)) $ do 
     putStrLn $
       "USAGE: "++this_progname++" [options] CSVFILE\n\n"++
       "Upload a pre-existing CSV data as gathered by the 'dribble' plugin.\n"++
       "\n"++
       (usageInfo "Options:" extra_cli_options)++"\n"++
       (usageInfo help fusion_cli_options)
     if null errs then exitSuccess else exitFailure

   let name = case [ n | TableName n <- opts1 ] of
               [] -> error "Must supply a table name!"
               [n] -> n
               ls  -> error $ "Multiple table names supplied!: "++show ls

   -- This bit could be abstracted nicely by the HSBencher lib:
   ------------------------------------------------------------
   -- Gather info about the benchmark platform:
   gconf0 <- getConfig [] []
   let gconf1 = gconf0 { benchsetName = Just name }
   gconf2 <- plugInitialize plug gconf1
   let fconf0 = getMyConf plug gconf2
   let fconf1 = foldFlags plug opts2 fconf0
   let gconf3 = setMyConf plug fconf1 gconf2
                
   ------------------------------------------------------------
   case plainargs of
     [] -> error "No file given to upload!"
     reports -> forM_ reports (doupload gconf3)

doupload :: Config -> FilePath -> IO ()
doupload confs file = do
  x <- parseCSVFromFile file
  case x of
    Left err -> error $ "Failed to read CSV file: \n"++show err
    Right [] -> error $ "Bad CSV file, not even a header line: "++file
    Right (hdr:rst) -> do
      checkHeader hdr
      mapM_ (uprow confs) (map (zip hdr) rst)

checkHeader :: Record -> IO ()
checkHeader hdr
  | L.elem "PROGNAME" hdr = return ()
  | otherwise = error $ "Bad HEADER line on CSV file: "++show hdr

uprow :: Config -> [(String,String)] -> IO ()
uprow gconf tuple  = do
  let br  = tupleToResult tuple
--  br' <- augmentResultWithConfig gconf br
  runReaderT (uploadBenchResult br) gconf
