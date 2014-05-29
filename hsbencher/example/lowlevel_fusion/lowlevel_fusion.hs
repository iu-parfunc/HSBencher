
import System.Directory
import System.FilePath
import System.Exit
import System.Environment (getArgs)

import HSBencher.Internal.Fusion 


import Prelude hiding (log,init )
import Data.List as L hiding (init) 
--------------------------------------------------------------------------------
-- recognizes these commandline args
-- --clientid=x
-- --clientsecret=y

main = do

  raw_args <- getArgs


  -- Very fragile! 
  (id,sec) <- case raw_args of
    [] -> error "No arguments"
    xs -> let cid_str = filter (\c -> "--clientid=" `L.isPrefixOf` c) raw_args 
              sec_str = filter (\c -> "--clientsecret=" `L.isPrefixOf` c) raw_args 
          in return (tail $ dropWhile (/= '=') (head cid_str),
                     tail $ dropWhile (/= '=') (head sec_str))

  putStrLn id
  putStrLn sec

  -- (_,columns) <- initialize id sec "newtable" 

  (tid,auth) <- init id sec "newtable" 

  putStrLn "Trying to get something out of a table \n \n" 

  str <- getSomething auth tid "MINTIME" :: IO String

  putStrLn str
  
  -- putStrLn $ unwords columns


  -- putStrLn $ show r 
--   putStrLn (head raw_args)
