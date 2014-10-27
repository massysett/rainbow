module Rainbow.Colors.Generators where

import qualified Rainbow.Colors as C
import Test.QuickCheck
import qualified Rainbow.Types.Generators as G
import Control.Monad
import qualified Prelude.Generators

both :: Gen C.Both
both = liftM2 C.Both G.color8
  (Prelude.Generators.maybe G.color256)
