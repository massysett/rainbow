module System.Console.Rainbow.Colors.Wrappers where

import qualified System.Console.Rainbow.Colors as C
import Test.QuickCheck
import qualified System.Console.Terminfo.Color.Wrappers as TW

newtype Color8 = Color8 { unColor8 :: C.Color8 }
  deriving (Eq, Ord, Show)

instance Arbitrary Color8 where
  arbitrary =
    fmap Color8
    . elements
    . (C.c8_default :)
    . map snd $ C.c8_all

instance CoArbitrary Color8 where
  coarbitrary (Color8 c) = case C.unColor8 c of
    Nothing -> variantI 0
    Just clr -> variantI 1 . coarbitrary (TW.Color clr)

variantI :: Int -> Gen a -> Gen a
variantI = variant

newtype Color256 = Color256 { unColor256 :: C.Color256 }
  deriving (Eq, Ord, Show)

instance Arbitrary Color256 where
  arbitrary =
    fmap Color256
    . elements
    . (C.c256_default :)
    . map snd $ C.c256_all

instance CoArbitrary Color256 where
  coarbitrary (Color256 c) = case C.unColor256 c of
    Nothing -> variantI 0
    Just clr -> variantI 1 . coarbitrary (TW.Color clr)
