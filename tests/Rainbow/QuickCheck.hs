{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, DeriveGeneric, StandaloneDeriving #-}

-- | QuickCheck instances for all of Rainbow.  Currently Rainbow does
-- not use these instances itself; they are only here for
-- cut-and-paste for other libraries that may need them.  There is an
-- executable in Rainbow that is built solely to make sure this module
-- compiles without any errors.
--
-- To use these instances, just drop them into your own project
-- somewhere.  They are not packaged as a library because there are
-- orphan instances.

module Rainbow.QuickCheck where

import Control.Applicative
import Test.QuickCheck
import Rainbow.Colors
import Rainbow.Types
import Data.Monoid
import Control.Monad
import qualified Data.Text as X

varInt :: Int -> Gen b -> Gen b
varInt = variant

instance Arbitrary Enum8 where
  arbitrary = elements [E0, E1, E2, E3, E4, E5, E6, E7]
  shrink = genericShrink

instance CoArbitrary Enum8 where
  coarbitrary x = case x of
    E0 -> varInt 0
    E1 -> varInt 1
    E2 -> varInt 2
    E3 -> varInt 3
    E4 -> varInt 4
    E5 -> varInt 5
    E6 -> varInt 6
    E7 -> varInt 7

instance Arbitrary Color8 where
  arbitrary = fmap Color8 arbitrary
  shrink = genericShrink

instance CoArbitrary Color8 where
  coarbitrary (Color8 Nothing) = varInt 0
  coarbitrary (Color8 (Just e)) = varInt 1 . coarbitrary e

instance Arbitrary Color256 where
  arbitrary = fmap Color256 arbitrary
  shrink = genericShrink

instance CoArbitrary Color256 where
  coarbitrary (Color256 Nothing) = varInt 0
  coarbitrary (Color256 (Just w)) = varInt 1 . coarbitrary w

instance Arbitrary (Last Color8) where
  arbitrary = fmap Last arbitrary

instance CoArbitrary (Last Color8) where
  coarbitrary (Last Nothing) = varInt 0
  coarbitrary (Last (Just c)) = varInt 1 . coarbitrary c

instance Arbitrary (Last Color256) where
  arbitrary = fmap Last arbitrary

instance CoArbitrary (Last Color256) where
  coarbitrary (Last Nothing) = varInt 0
  coarbitrary (Last (Just c)) = varInt 1 . coarbitrary c

instance Arbitrary (Last Bool) where
  arbitrary = fmap Last arbitrary

instance CoArbitrary (Last Bool) where
  coarbitrary (Last Nothing) = varInt 0
  coarbitrary (Last (Just b)) = varInt 1 . coarbitrary b

instance Arbitrary StyleCommon where
  arbitrary
    = StyleCommon <$> g <*> g <*> g <*> g <*> g <*> g <*> g <*> g
    where
      g = fmap Last arbitrary

instance CoArbitrary StyleCommon where
  coarbitrary (StyleCommon x0 x1 x2 x3 x4 x5 x6 x7)
    = coarbitrary x0
    . coarbitrary x1
    . coarbitrary x2
    . coarbitrary x3
    . coarbitrary x4
    . coarbitrary x5
    . coarbitrary x6
    . coarbitrary x7
    

instance Arbitrary Style256 where
  arbitrary = liftM3 Style256 arbitrary arbitrary arbitrary

instance CoArbitrary Style256 where
  coarbitrary (Style256 x0 x1 x2)
    = coarbitrary x0
    . coarbitrary x1
    . coarbitrary x2

instance Arbitrary Style8 where
  arbitrary = liftM3 Style8 arbitrary arbitrary arbitrary

instance CoArbitrary Style8 where
  coarbitrary (Style8 x0 x1 x2)
    = coarbitrary x0
    . coarbitrary x1
    . coarbitrary x2

instance Arbitrary TextSpec where
  arbitrary = liftM2 TextSpec arbitrary arbitrary
  shrink = genericShrink

instance CoArbitrary TextSpec where
  coarbitrary (TextSpec x0 x1)
    = coarbitrary x0
    . coarbitrary x1

instance Arbitrary X.Text where
  arbitrary = fmap X.pack arbitrary
  shrink = map X.pack . shrink . X.unpack

instance Arbitrary Chunk where
  arbitrary = liftM2 Chunk arbitrary
    (listOf (fmap X.pack (listOf (elements ['a'..'z']))))
  shrink = genericShrink

instance CoArbitrary Chunk where
  coarbitrary (Chunk ts txts) = coarbitrary ts
    . coarbitrary (map X.unpack txts)

instance Arbitrary Radiant where
  arbitrary = liftM2 Radiant arbitrary arbitrary
  shrink = genericShrink

instance CoArbitrary Radiant where
  coarbitrary (Radiant x0 x1)
    = coarbitrary x0
    . coarbitrary x1
