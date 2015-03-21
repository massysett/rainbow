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

instance Arbitrary Enum8 where
  arbitrary = elements [E0, E1, E2, E3, E4, E5, E6, E7]
  shrink = genericShrink

instance CoArbitrary Enum8

instance Arbitrary Color8 where
  arbitrary = fmap Color8 arbitrary
  shrink = genericShrink

instance CoArbitrary Color8

instance Arbitrary Color256 where
  arbitrary = fmap Color256 arbitrary
  shrink = genericShrink

instance CoArbitrary Color256

instance Arbitrary (Last Color8) where
  arbitrary = fmap Last arbitrary
  shrink = genericShrink

instance CoArbitrary (Last Color8)

instance Arbitrary (Last Color256) where
  arbitrary = fmap Last arbitrary
  shrink = genericShrink

instance CoArbitrary (Last Color256)

instance Arbitrary (Last Bool) where
  arbitrary = fmap Last arbitrary
  shrink = genericShrink

instance CoArbitrary (Last Bool)

instance Arbitrary StyleCommon where
  arbitrary
    = StyleCommon <$> g <*> g <*> g <*> g <*> g <*> g <*> g <*> g
    where
      g = fmap Last arbitrary
  shrink = genericShrink

instance CoArbitrary StyleCommon

instance Arbitrary Style256 where
  arbitrary = liftM3 Style256 arbitrary arbitrary arbitrary
  shrink = genericShrink

instance CoArbitrary Style256

instance Arbitrary Style8 where
  arbitrary = liftM3 Style8 arbitrary arbitrary arbitrary
  shrink = genericShrink

instance CoArbitrary Style8

instance Arbitrary TextSpec where
  arbitrary = liftM2 TextSpec arbitrary arbitrary
  shrink = genericShrink

instance CoArbitrary TextSpec

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

instance CoArbitrary Radiant
