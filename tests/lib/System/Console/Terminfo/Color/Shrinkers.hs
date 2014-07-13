module System.Console.Terminfo.Color.Shrinkers where

import qualified System.Console.Terminfo.Color.Generators as GC
import qualified System.Console.Terminfo.Color as C
import Test.QuickCheck

color :: C.Color -> [C.Color]
color c = case c of
  C.ColorNumber n -> GC.basicColors ++
    map C.ColorNumber (shrinkIntegral n)
  _ -> []
