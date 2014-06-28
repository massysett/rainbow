{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module System.Console.Rainbow.Types.Wrappers where

import Prelude hiding (last)
import Data.Monoid
import Test.QuickCheck
import qualified System.Console.Rainbow.Types as T
import Control.Monad
import System.Console.Rainbow.Colors.Wrappers
import Data.Text.Wrappers
import qualified Data.Text as X
import Test.SmallCheck.Series
import qualified Data.Monoid.Wrappers as MW

last :: Gen a -> Gen (Last a)
last g = fmap Last $
  frequency [(3, fmap Just g), (1, return Nothing)]

newtype StyleCommon = StyleCommon { unStyleCommon :: T.StyleCommon }
  deriving (Eq, Ord, Show)

instance Arbitrary StyleCommon where
  arbitrary = fmap StyleCommon $
    liftM4 T.StyleCommon (last arbitrary) (last arbitrary)
      (last arbitrary) (last arbitrary)

instance Monad m => Serial m StyleCommon where
  series = fmap StyleCommon $ cons4 f
    where
      f (MW.Last a) (MW.Last b) (MW.Last c) (MW.Last d) =
        T.StyleCommon a b c d

instance Monad m => CoSerial m StyleCommon where
  coseries rs =
    alts4 rs >>- \f ->
    return $ \(StyleCommon (T.StyleCommon a b c d)) ->
      f (MW.Last a) (MW.Last b) (MW.Last c) (MW.Last d)

newtype Style8 = Style8 { unStyle8 :: T.Style8 }
  deriving (Eq, Ord, Show)

instance Monad m => Serial m Style8 where
  series = fmap Style8 $ cons3 f
    where
      f f8 b8 (StyleCommon sc) =
        T.Style8 (Last (fmap unColor8 f8)) (Last (fmap unColor8 b8)) sc

instance Monad m => CoSerial m Style8 where
  coseries rs =
    alts3 rs >>- \f3 ->
    return $ \(Style8 (T.Style8 (Last f8) (Last b8) sc)) ->
    f3 (fmap Color8 f8) (fmap Color8 b8) (StyleCommon sc)

instance Arbitrary Style8 where
  arbitrary = fmap Style8 $
    liftM3 T.Style8 (last (fmap unColor8 arbitrary))
      (last (fmap unColor8 arbitrary)) (fmap unStyleCommon arbitrary)

newtype Style256 = Style256 { unStyle256 :: T.Style256 }
  deriving (Eq, Ord, Show)

instance Monad m => Serial m Style256 where
  series = fmap Style256 $ cons3 f
    where
      f f256 b256 (StyleCommon sc) =
        T.Style256 (Last (fmap unColor256 f256))
                   (Last (fmap unColor256 b256)) sc

instance Monad m => CoSerial m Style256 where
  coseries rs =
    alts3 rs >>- \f3 ->
    return $ \(Style256 (T.Style256 (Last f256) (Last b256) sc)) ->
    f3 (fmap Color256 f256) (fmap Color256 b256) (StyleCommon sc)

instance Arbitrary Style256 where
  arbitrary = fmap Style256 $
    liftM3 T.Style256 (last (fmap unColor256 arbitrary))
      (last (fmap unColor256 arbitrary)) (fmap unStyleCommon arbitrary)

newtype TextSpec = TextSpec { unTextSpec :: T.TextSpec }
  deriving (Eq, Ord, Show)

instance Monad m => Serial m TextSpec where
  series = fmap TextSpec $ cons2 f
    where
      f (Style8 s8) (Style256 s256) = T.TextSpec s8 s256

instance Monad m => CoSerial m TextSpec where
  coseries rs =
    alts2 rs >>- \f2 ->
    return $ \(TextSpec (T.TextSpec s8 s256)) ->
    f2 (Style8 s8) (Style256 s256)

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

instance Monad m => Serial m Chunk where
  series = fmap Chunk $ cons2 f
    where
      f (TextSpec ts) xs = T.Chunk ts (map unText xs)

instance Monad m => CoSerial m Chunk where
  coseries rs =
    alts2 rs >>- \f2 ->
    return $ \(Chunk (T.Chunk ts t)) ->
    f2 (TextSpec ts) (map Text t)
