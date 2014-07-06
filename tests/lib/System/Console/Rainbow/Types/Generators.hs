module System.Console.Rainbow.Types.Generators where

import Data.Monoid.Generators
import qualified System.Console.Rainbow.Types as T
import System.Console.Rainbow.Colors.Generators
import Test.QuickCheck
import Control.Monad
import Data.Text.Generators
import Prelude ()

styleCommon :: Gen T.StyleCommon
styleCommon = liftM4 T.StyleCommon g g g g
  where
    g = last arbitrary

style8 :: Gen T.Style8
style8 = liftM3 T.Style8 g g styleCommon
  where
    g = last color8

style256 :: Gen T.Style256
style256 = liftM3 T.Style256 g g styleCommon
  where
    g = last color256

textSpec :: Gen T.TextSpec
textSpec = liftM2 T.TextSpec style8 style256

chunk :: Gen T.Chunk
chunk = liftM2 T.Chunk textSpec (listOf (text arbitrary))
