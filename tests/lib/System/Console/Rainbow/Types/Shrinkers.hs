module System.Console.Rainbow.Types.Shrinkers where

import Data.Text.Shrinkers
import Rainbow.Tests.Util
import Data.Monoid.Shrinkers
import System.Console.Rainbow.Colors.Shrinkers
import Test.QuickCheck
import qualified System.Console.Rainbow.Types as T
import Prelude ()

styleCommon :: T.StyleCommon -> [T.StyleCommon]
styleCommon (T.StyleCommon a b c d) =
  [ T.StyleCommon a' b' c' d'
    | (a', b', c', d') <- shrink4 s s s s (a, b, c, d) ]
  where
    s = last shrink

style8 :: T.Style8 -> [T.Style8]
style8 (T.Style8 f8 b8 c) =
  [ T.Style8 f8' b8' c'
  | (f8', b8', c') <- shrink3 sc sc styleCommon (f8, b8, c) ]
  where
    sc = last color8

style256 :: T.Style256 -> [T.Style256]
style256 (T.Style256 f256 b256 c) =
  [ T.Style256 f256' b256' c'
  | (f256', b256', c') <- shrink3 sc sc styleCommon (f256, b256, c) ]
  where
    sc = last color256

textSpec :: T.TextSpec -> [T.TextSpec]
textSpec (T.TextSpec s8 s256) =
  [ T.TextSpec s8' s256'
  | (s8', s256') <- shrink2 style8 style256 (s8, s256) ]

chunk :: T.Chunk -> [T.Chunk]
chunk (T.Chunk t xs) = [T.Chunk t' xs'
  | (t', xs') <- shrink2 textSpec (shrinkList (text shrink)) (t, xs) ]
