module Data.Text.Shrinkers where

import Data.Text (Text)
import qualified Data.Text as X
import Test.QuickCheck

text :: (Char -> [Char]) -> Text -> [Text]
text f = map X.pack . shrinkList f . X.unpack
