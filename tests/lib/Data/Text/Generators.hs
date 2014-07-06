module Data.Text.Generators where

import Test.QuickCheck
import qualified Data.Text as X
import Data.Text (Text)

text :: Gen Char -> Gen Text
text = fmap X.pack . listOf

