module Rainbow.Colors.Coarbitrary where

import Test.QuickCheck
import Rainbow.Colors
import Rainbow.Types.Coarbitrary
import qualified Prelude.Coarbitrary

both :: Both -> Gen b -> Gen b
both (Both c8 mc256) =
  color8 c8
  . Prelude.Coarbitrary.maybe color256 mc256

