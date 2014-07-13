module System.Console.Terminfo.Color.Coarbitrary where

import Test.QuickCheck
import System.Console.Terminfo.Color

color :: Color -> Gen b -> Gen b
color c = case c of
  Black -> var 0
  Red -> var 1
  Green -> var 2
  Yellow -> var 3
  Blue -> var 4
  Magenta -> var 5
  Cyan -> var 6
  White -> var 7
  ColorNumber i -> var 8 . var i
  where
    var = variant
