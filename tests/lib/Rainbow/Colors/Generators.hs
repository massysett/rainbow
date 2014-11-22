module Rainbow.Colors.Generators where

import qualified Rainbow.Colors as C
import Test.QuickCheck
import qualified Rainbow.Types.Generators as G
import Control.Monad
import qualified Prelude.Generators

radiant :: Gen C.Radiant
radiant = liftM2 C.Radiant G.color8
  (Prelude.Generators.maybe G.color256)
