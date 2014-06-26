module System.Console.Terminfo.Color.Wrappers where

import qualified System.Console.Terminfo.Color as C
import Test.QuickCheck

newtype Color = Color { unColor :: C.Color }
  deriving (Eq, Ord, Show)

-- | Assumes the terminal can display 256 colors.
instance Arbitrary Color where
  arbitrary = fmap Color . elements $
    [ C.Black
    , C.Red
    , C.Green
    , C.Yellow
    , C.Blue
    , C.Magenta
    , C.Cyan
    , C.White
    ] ++ map C.ColorNumber [0..255]

instance CoArbitrary Color where
  coarbitrary (Color c) = case c of
    C.Black -> variantI 0
    C.Red -> variantI 1
    C.Green -> variantI 2
    C.Yellow -> variantI 3
    C.Blue -> variantI 4
    C.Magenta -> variantI 5
    C.Cyan -> variantI 6
    C.White -> variantI 7
    C.ColorNumber i -> variantI 8 . variantI i
    where
      variantI :: Int -> Gen a -> Gen a
      variantI = variant
