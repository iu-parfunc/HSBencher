

-- | Convenience module that reexports the necessary bits.

module HSBencher
       (
         -- module HSBencher.App,
         -- Reproducing this here to get around the limitation of haddock 0.2 that it
         -- won't list all the bindings for reexports.

         -- * The main entrypoints for building new benchmark suites.
         defaultMainModifyConfig,
         addPlugin, addBenchmarks,

         -- * Command-line configuration
         Flag(..),
         all_cli_options, fullUsageInfo,

         -- * All the types necessary for configuration and customization

         -- | Don't import the module below directly, but do click on this link to
         -- read its documentation.
         module HSBencher.Types,

         module HSBencher.Harvesters
       )
       where

import HSBencher.Types
import HSBencher.Internal.App
import HSBencher.Harvesters
import HSBencher.Internal.Config (addPlugin)

-- | A convenience combinator for composing together with other
-- `Config` transformers.  This one adds benchmarks to whatever is
-- already in the `benchlist` field.
addBenchmarks :: [Benchmark DefaultParamMeaning] -> Config -> Config
addBenchmarks ls conf = conf { benchlist = ls ++ benchlist conf } 
