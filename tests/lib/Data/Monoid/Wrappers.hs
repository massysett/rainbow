{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.Monoid.Wrappers where

import qualified Data.Monoid as O
import Test.SmallCheck.Series

newtype Last a = Last { unLast :: O.Last a }
  deriving (Eq, Show, Ord)

instance Functor Last where
  fmap f (Last (O.Last a)) = Last (O.Last (fmap f a))

instance Serial m a => Serial m (Last a) where
  series = fmap Last $ cons0 (O.Last Nothing) \/ cons1 (O.Last . Just)

instance CoSerial m a => CoSerial m (Last a) where
  coseries rs =
    alts0 rs >>- \f0 ->
    alts1 rs >>- \f1 ->
    return $ \(Last (O.Last m)) -> case m of
      Nothing -> f0
      Just a -> f1 a
