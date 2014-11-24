module Rainbow.Types.Shrinkers where

import Data.Text.Shrinkers
import Data.Monoid.Shrinkers
import Test.QuickCheck
  ( shrinkIntegral, shrink )

-- be sure to import shrinkList from Test.QuickCheck.Arbitrary to
-- maintain compatibility between QuickCheck 2.6 and 2.7
import Test.QuickCheck.Arbitrary ( shrinkList )
import qualified Rainbow.Types as T
import Prelude hiding (last)
import Prelude.Shrinkers

enum8 :: T.Enum8 -> [T.Enum8]
enum8 e = case e of
  T.E0 -> []
  T.E1 -> [T.E0]
  T.E2 -> [T.E0, T.E1]
  T.E3 -> [T.E0, T.E1, T.E2]
  T.E4 -> [T.E0, T.E1, T.E2, T.E3]
  T.E5 -> [T.E0, T.E1, T.E2, T.E3, T.E4]
  T.E6 -> [T.E0, T.E1, T.E2, T.E3, T.E4, T.E5]
  T.E7 -> [T.E0, T.E1, T.E2, T.E3, T.E4, T.E5, T.E6]

color8 :: T.Color8 -> [T.Color8]
color8 (T.Color8 me) = case me of
  Nothing -> []
  Just e -> map T.Color8 $ Nothing : map Just (enum8 e)

color256 :: T.Color256 -> [T.Color256]
color256 (T.Color256 me) = case me of
  Nothing -> []
  Just e -> map T.Color256 $ Nothing : map Just (shrinkIntegral e)

styleCommon :: T.StyleCommon -> [T.StyleCommon]
styleCommon (T.StyleCommon a b c d) =
  [ T.StyleCommon a' b' c' d'
    | (a', b', c', d') <- tuple4 s s s s (a, b, c, d) ]
  where
    s = last shrink

style8 :: T.Style8 -> [T.Style8]
style8 (T.Style8 f8 b8 c) =
  [ T.Style8 f8' b8' c'
  | (f8', b8', c') <- tuple3 sc sc styleCommon (f8, b8, c) ]
  where
    sc = last color8

style256 :: T.Style256 -> [T.Style256]
style256 (T.Style256 f256 b256 c) =
  [ T.Style256 f256' b256' c'
  | (f256', b256', c') <- tuple3 sc sc styleCommon (f256, b256, c) ]
  where
    sc = last color256

textSpec :: T.TextSpec -> [T.TextSpec]
textSpec (T.TextSpec s8 s256) =
  [ T.TextSpec s8' s256'
  | (s8', s256') <- tuple2 style8 style256 (s8, s256) ]

chunk :: T.Chunk -> [T.Chunk]
chunk (T.Chunk t xs) = [T.Chunk t' xs'
  | (t', xs') <- tuple2 textSpec (shrinkList (text shrink)) (t, xs) ]
