

-- | Example program

import System.Environment
import System.Process
import System.IO
import Control.Concurrent

main = do
  putStrLn "Running simple ls command after sleeping."
  hFlush stdout
  threadDelay $ 3000 * 1000
  putStrLn "Done sleeping:"  
  args <- getArgs
  rawSystem "ls" args

  putStrLn "SELFTIMED 99.9"

