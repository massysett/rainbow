{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.Text.Wrappers where

import qualified Data.Text as X
import Test.SmallCheck.Series
import Test.QuickCheck

newtype Text = Text { unText :: X.Text }
  deriving (Eq, Ord, Show)

instance Monad m => Serial m Text where
  series = fmap (Text . X.pack) series

instance Monad m => CoSerial m Text where
  coseries rs = newtypeAlts rs >>- \f ->
    return $ \(Text x) -> f (X.unpack x)

instance Arbitrary Text where
  arbitrary = fmap (Text . X.pack) arbitrary
