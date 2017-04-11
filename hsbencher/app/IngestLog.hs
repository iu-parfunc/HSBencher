{-# LANGUAGE NamedFieldPuns #-}

-- | Simple application to parse logs.

module Main where

import           Options.Applicative
import           System.FilePath
import           Data.Monoid
import           Data.Maybe
    
data Opts = Opts { infile  :: Maybe FilePath
                 , outfile :: Maybe FilePath
                 }
 deriving (Eq,Show,Read,Ord)
          
argsParser :: Parser Opts
argsParser = Opts 
             <$> optional (strArgument (metavar "InputFile.log"))
             <*> optional (strArgument (metavar "OutputFile.csv"))

-- https://github.com/iu-parfunc/HSBencher/issues/75
    
main :: IO ()
main = do opts <- execParser $ info (helper <*> argsParser)
                  ( fullDesc
                  <> progDesc (unlines
                               [ "Utility to ingest logs and produce HSBencher-formatted CSV output."
                               , "As described in: https://github.com/iu-parfunc/HSBencher/issues/75"
                               , "Writes to the specified input/output files, if present, or from/to stdin/stdout otherwise."
                               ])
                  <> header "hsbencher-ingest-log" )
          ingestLog opts

ingestLog :: Opts -> IO ()
ingestLog opts@Opts{infile,outfile} = do
  let input = fromMaybe "/dev/stdin" infile
  putStrLn $ "hello "++show opts
