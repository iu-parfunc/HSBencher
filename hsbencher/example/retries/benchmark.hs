

import HSBencher
import HSBencher.Backend.Dribble (defaultDribblePlugin)
import Data.Default (Default(def))

main :: IO ()
main = defaultMainModifyConfig $
    (addPlugin defaultDribblePlugin def) . 
    (addBenchmarks benches)

benches :: [Benchmark DefaultParamMeaning]
benches =
  [ mkBenchmark "bench1/" [ ] noParams
  ]

-- No benchmark configuration space.
noParams :: BenchSpace DefaultParamMeaning
noParams = And []
