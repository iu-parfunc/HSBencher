

-- | Example program

import System.Environment
import System.Process
import Control.Concurrent

main = do
  putStrLn "Running simple ls command after sleeping."
  threadDelay $ 3000 * 1000
  putStrLn "Done sleeping:"  
  args <- getArgs
  rawSystem "ls" args

