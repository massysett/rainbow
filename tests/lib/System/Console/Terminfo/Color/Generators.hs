module System.Console.Terminfo.Color.Generators where

import Test.QuickCheck
import qualified System.Console.Terminfo.Color as C

basicColors :: [C.Color]
basicColors =
  [ C.Black
  , C.Red
  , C.Green
  , C.Yellow
  , C.Blue
  , C.Magenta
  , C.Cyan
  , C.White
  ]

allColors :: [C.Color]
allColors = basicColors ++ map C.ColorNumber [0..255]

color :: Gen C.Color
color = elements allColors
