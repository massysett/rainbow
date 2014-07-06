module Data.Text.Coarbitrary where

import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as X

text :: Text -> Gen b -> Gen b
text = coarbitrary . X.unpack
