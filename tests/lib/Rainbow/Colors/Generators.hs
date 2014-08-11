module Rainbow.Colors.Generators where

import qualified Rainbow.Colors as C
import Test.QuickCheck
import qualified Rainbow.Types.Generators as G

both :: Gen C.Both
both = fmap C.Both G.color8
