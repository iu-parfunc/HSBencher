

import HSBencher
import HSBencher.Backend.Dribble (defaultDribblePlugin)
import Data.Default (Default(def))

main :: IO ()
-- main = defaultMainWithBechmarks benches
main = defaultMainModifyConfig $ \ conf -> 
    addPlugin defaultDribblePlugin def $ 
    conf { benchlist= benches }

benches :: [Benchmark DefaultParamMeaning]
benches =
  [ mkBenchmark "bench1/" [ ] noParams
  ]

-- No benchmark configuration space.
noParams :: BenchSpace DefaultParamMeaning
noParams = And []
