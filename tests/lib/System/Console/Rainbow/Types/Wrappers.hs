module System.Console.Rainbow.Types.Wrappers where

import Prelude hiding (last)
import Data.Monoid
import Test.QuickCheck
import qualified System.Console.Rainbow.Types as T
import Control.Monad
import System.Console.Rainbow.Colors.Wrappers
import qualified Data.Text as X

last :: Gen a -> Gen (Last a)
last g = fmap Last $
  frequency [(3, fmap Just g), (1, return Nothing)]

newtype StyleCommon = StyleCommon { unStyleCommon :: T.StyleCommon }
  deriving (Eq, Ord, Show)

instance Arbitrary StyleCommon where
  arbitrary = fmap StyleCommon $
    liftM4 T.StyleCommon (last arbitrary) (last arbitrary)
      (last arbitrary) (last arbitrary)

newtype Style8 = Style8 { unStyle8 :: T.Style8 }
  deriving (Eq, Ord, Show)

instance Arbitrary Style8 where
  arbitrary = fmap Style8 $
    liftM3 T.Style8 (last (fmap unColor8 arbitrary))
      (last (fmap unColor8 arbitrary)) (fmap unStyleCommon arbitrary)

newtype Style256 = Style256 { unStyle256 :: T.Style256 }
  deriving (Eq, Ord, Show)

instance Arbitrary Style256 where
  arbitrary = fmap Style256 $
    liftM3 T.Style256 (last (fmap unColor256 arbitrary))
      (last (fmap unColor256 arbitrary)) (fmap unStyleCommon arbitrary)

newtype TextSpec = TextSpec { unTextSpec :: T.TextSpec }
  deriving (Eq, Ord, Show)

instance Arbitrary TextSpec where
  arbitrary = fmap TextSpec $ liftM2 T.TextSpec
    (fmap unStyle8 arbitrary) (fmap unStyle256 arbitrary)

newtype Chunk = Chunk { unChunk :: T.Chunk }
  deriving (Eq, Ord, Show)

instance Arbitrary Chunk where
  arbitrary = fmap Chunk $ liftM2 T.Chunk
    (fmap unTextSpec arbitrary) (listOf genText)
    where
      genText = fmap X.pack $ listOf arbitrary
