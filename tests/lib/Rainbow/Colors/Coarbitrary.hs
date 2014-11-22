module Rainbow.Colors.Coarbitrary where

import Test.QuickCheck
import Rainbow.Colors
import Rainbow.Types.Coarbitrary
import qualified Prelude.Coarbitrary

radiant :: Radiant -> Gen b -> Gen b
radiant (Radiant c8 mc256) =
  color8 c8
  . Prelude.Coarbitrary.maybe color256 mc256

