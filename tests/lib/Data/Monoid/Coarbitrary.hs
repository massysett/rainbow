module Data.Monoid.Coarbitrary where

import Test.QuickCheck
import qualified Data.Monoid as O

last :: (a -> Gen b -> Gen b) -> O.Last a -> Gen b -> Gen b
last f (O.Last m) = case m of
  Nothing -> variant (0 :: Int)
  Just a -> variant (1 :: Int) . f a
