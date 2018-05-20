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

import Test.QuickCheck
import Rainbow.Types
import Data.Typeable

instance (Typeable a, Arbitrary a) => Arbitrary (Color a) where
  arbitrary = Color <$> arbitrary
  shrink = genericShrink

instance CoArbitrary a => CoArbitrary (Color a) where
  coarbitrary (Color a) = coarbitrary a

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

instance Arbitrary Format where
  arbitrary
    = Format <$> g <*> g <*> g <*> g <*> g <*> g <*> g <*> g
    where
      g = arbitrary
  shrink = genericShrink

instance CoArbitrary Format where
  coarbitrary (Format x0 x1 x2 x3 x4 x5 x6 x7)
    = coarbitrary x0
    . coarbitrary x1
    . coarbitrary x2
    . coarbitrary x3
    . coarbitrary x4
    . coarbitrary x5
    . coarbitrary x6
    . coarbitrary x7

instance (Arbitrary a, Typeable a) => Arbitrary (Style a) where
  arbitrary = Style <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance CoArbitrary a => CoArbitrary (Style a) where
  coarbitrary (Style a b c)
    = coarbitrary a
    . coarbitrary b
    . coarbitrary c

instance Arbitrary Scheme where
  arbitrary = Scheme <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance CoArbitrary Scheme where
  coarbitrary (Scheme a b) = coarbitrary a . coarbitrary b


instance (Arbitrary a, Typeable a) => Arbitrary (Chunk a) where
  arbitrary = Chunk <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance CoArbitrary a => CoArbitrary (Chunk a) where
  coarbitrary (Chunk a b)
    = coarbitrary a
    . coarbitrary b

instance Arbitrary Radiant where
  arbitrary = Radiant <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance CoArbitrary Radiant where
  coarbitrary (Radiant a b) = coarbitrary a . coarbitrary b

