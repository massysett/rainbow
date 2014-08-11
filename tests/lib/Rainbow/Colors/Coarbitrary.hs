module Rainbow.Colors.Coarbitrary where

import Test.QuickCheck
import Rainbow.Colors
import Rainbow.Types.Coarbitrary

both :: Both -> Gen b -> Gen b
both (Both c) = color8 c

