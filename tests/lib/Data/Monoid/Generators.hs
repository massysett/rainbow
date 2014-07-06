module Data.Monoid.Generators where

import qualified Data.Monoid as O
import Test.QuickCheck

last :: Gen a -> Gen (O.Last a)
last g = fmap O.Last $ frequency
  [ (3, fmap Just g), (1, return Nothing) ]
