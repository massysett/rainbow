module Rainbow.Types.Generators where

import Data.Monoid.Generators
import qualified Rainbow.Types as T
import Test.QuickCheck
import Control.Monad
import Data.Text.Generators
import Prelude.Generators
import Prelude hiding (last, maybe)


enum8 :: Gen T.Enum8
enum8 = elements [minBound..maxBound]

color8 :: Gen T.Color8
color8 = fmap T.Color8 $ maybe enum8

color256 :: Gen T.Color256
color256 = fmap T.Color256 $ maybe (elements [minBound..maxBound])

background8 :: Gen T.Background8
background8 = last color8

background256 :: Gen T.Background256
background256 = last color256

foreground8 :: Gen T.Foreground8
foreground8 = last color8

foreground256 :: Gen T.Foreground256
foreground256 = last color256

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
