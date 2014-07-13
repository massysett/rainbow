module Data.Monoid.Shrinkers where

import qualified Data.Monoid as O

last :: (a -> [a]) -> O.Last a -> [O.Last a]
last f (O.Last l) = case l of
  Nothing -> []
  Just a -> map O.Last $ Nothing : [Just x | x <- f a]
