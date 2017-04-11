{-# LANGUAGE ScopedTypeVariables #-} 

import System.Environment
import System.Random
 

main = do 

  args <- getArgs

  case args of
    [name,threads] ->
      do
        --g <- getStdGen
        --let (r :: Double,_) = random g

        putStrLn $ "NAME: " ++ name
        putStrLn $ "NUMTHREADS: " ++ threads
        putStrLn $ "SELFTIMED: " ++ show (1000 / (read threads :: Double) )
        
    _ -> error "Provide 2 argument"
  
