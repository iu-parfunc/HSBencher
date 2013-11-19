

-- | Convenience module that reexports the necessary bits.

module HSBencher
       (
         -- module HSBencher.App,

         -- Reproducing this here to get around the limitation of haddock 0.2 that it
         -- won't list all the bindings for reexports.

         -- * The main entrypoints for building new benchmark suites.
         defaultMainWithBechmarks, defaultMainModifyConfig,
         Flag(..), all_cli_options,

         -- * All the types necessary for configuration
         module HSBencher.Types
       )
       where

import HSBencher.App
import HSBencher.Types
