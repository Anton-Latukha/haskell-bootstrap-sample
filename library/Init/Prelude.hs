-- | This is a @Prelude@, but, please, do not put things in here,
-- put them into "Init.Utils". This module is a pass-through-multiplexer,
-- between our custom code (Init.Utils") that shadows over the outside prelude that is in use ("Relude")
-- "Prelude" module has a problem of being imported & used by other projects.
-- "Nix.Utils" as a module with a regular name does not have that problem.
module Init.Prelude
    ( module Init.Utils
    , module Relude
    ) where

import           Init.Utils
import           Relude                  hiding ( pass
                                                , force
                                                , readFile
                                                , whenJust
                                                , whenNothing
                                                , trace
                                                , traceM
                                                )
