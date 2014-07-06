module Rainbow.Tests.Util where

import Test.QuickCheck

shrink2 :: (a -> [a]) -> (b -> [b]) -> (a, b) -> [(a, b)]
shrink2 fa fb (a, b) =
     [ (a', b) | a' <- fa a ]
  ++ [ (a, b') | b' <- fb b ]

shrink3
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> (a, b, c)
  -> [(a, b, c)]
shrink3 fa fb fc (a, b, c) =
  [ (a', b', c') | (a', (b', c'))
    <- shrink2 fa (shrink2 fb fc) (a, (b, c)) ]

shrink4
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> (d -> [d])
  -> (a, b, c, d)
  -> [(a, b, c, d)]
shrink4 fa fb fc fd (a, b, c, d) =
  [ (a', b', c', d') | (a', (b', (c', d')))
    <- shrink2 fa (shrink2 fb (shrink2 fc fd)) (a, (b, (c, d))) ]

shrink5
  :: (a -> [a])
  -> (b -> [b])
  -> (c -> [c])
  -> (d -> [d])
  -> (e -> [e])
  -> (a, b, c, d, e)
  -> [(a, b, c, d, e)]
shrink5 fa fb fc fd fe (a, b, c, d, e) =
  [ (a', b', c', d', e') | (a', (b', (c', (d', e'))))
    <- shrink2 fa (shrink2 fb (shrink2 fc (shrink2 fd fe)))
                  (a, (b, (c, (d, e)))) ]

coarbitraryList
  :: (a -> Gen b -> Gen b)
  -> [a]
  -> Gen b
  -> Gen b
coarbitraryList f xs = case xs of
  [] -> variant (0 :: Int)
  a:as -> variant (1 :: Int) . f a . coarbitraryList f as

