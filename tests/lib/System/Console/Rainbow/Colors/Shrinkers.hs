module System.Console.Rainbow.Colors.Shrinkers where

import Test.QuickCheck
import qualified System.Console.Rainbow.Colors as C
import qualified System.Console.Terminfo.Color as TC

color8Named :: [C.Color8]
color8Named =
  [ C.c8_black
  , C.c8_red
  , C.c8_green
  , C.c8_yellow
  , C.c8_blue
  , C.c8_magenta
  , C.c8_cyan
  , C.c8_white
  ]

color256Named :: [C.Color256]
color256Named =
  [ C.c256_black
  , C.c256_red
  , C.c256_green
  , C.c256_yellow
  , C.c256_blue
  , C.c256_magenta
  , C.c256_cyan
  , C.c256_white
  ]

color8 :: C.Color8 -> [C.Color8]
color8 c = case C.unColor8 c of
  Nothing -> []
  Just t -> case t of
    TC.ColorNumber n -> C.c8_default : color8Named ++ rest
      where
        rest = map get . shrinkIntegral $ n
        get i = case lookup i C.c8_all of
          Nothing -> error "color8 shrink failed"
          Just x -> x
    _ -> [C.c8_default]

color256 :: C.Color256 -> [C.Color256]
color256 c = case C.unColor256 c of
  Nothing -> []
  Just t -> case t of
    TC.ColorNumber n -> C.c256_default : color256Named ++ rest
      where
        rest = map get . shrinkIntegral $ n
        get i = case lookup i C.c256_all of
          Nothing -> error "color256 shrink failed"
          Just x -> x
    _ -> [C.c256_default]

