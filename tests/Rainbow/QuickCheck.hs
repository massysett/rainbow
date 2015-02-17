{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Rainbow.Colors
import Rainbow.Types
import Data.Monoid
import Control.Monad
import qualified Data.Text as X

instance Arbitrary Enum8 where
  arbitrary = elements [E0, E1, E2, E3, E4, E5, E6, E7]

instance Arbitrary Color8 where
  arbitrary = fmap Color8 arbitrary

instance Arbitrary Color256 where
  arbitrary = fmap Color256 arbitrary

instance Arbitrary (Last Color8) where
  arbitrary = fmap Last arbitrary

instance Arbitrary (Last Color256) where
  arbitrary = fmap Last arbitrary

instance Arbitrary StyleCommon where
  arbitrary
    = liftM4 StyleCommon g g g g
    where
      g = fmap Last arbitrary

instance Arbitrary Style256 where
  arbitrary = liftM3 Style256 arbitrary arbitrary arbitrary

instance Arbitrary Style8 where
  arbitrary = liftM3 Style8 arbitrary arbitrary arbitrary

instance Arbitrary TextSpec where
  arbitrary = liftM2 TextSpec arbitrary arbitrary

instance Arbitrary Chunk where
  arbitrary = liftM2 Chunk arbitrary
    (listOf (fmap X.pack (listOf (elements ['a'..'z']))))

instance Arbitrary Radiant where
  arbitrary = liftM2 Radiant arbitrary arbitrary
