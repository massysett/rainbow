module System.Console.Rainbow.Colors.Generators where

import qualified System.Console.Rainbow.Colors as C
import Test.QuickCheck

color8 :: Gen C.Color8
color8 = elements $ C.c8_default : map snd C.c8_all

color256 :: Gen C.Color256
color256 = elements $ C.c256_default : map snd C.c256_all
