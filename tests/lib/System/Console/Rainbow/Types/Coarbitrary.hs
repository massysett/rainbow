module System.Console.Rainbow.Types.Coarbitrary where

import Test.QuickCheck
import Data.Monoid.Coarbitrary
import Data.Text.Coarbitrary
import System.Console.Rainbow.Colors.Coarbitrary
import qualified System.Console.Rainbow.Types as T
import Rainbow.Tests.Util
import Prelude hiding (last)

styleCommon :: T.StyleCommon -> Gen b -> Gen b
styleCommon c
  = last coarbitrary (T.scBold c)
  . last coarbitrary (T.scUnderline c)
  . last coarbitrary (T.scFlash c)
  . last coarbitrary (T.scInverse c)

style8 :: T.Style8 -> Gen b -> Gen b
style8 c
  = last color8 (T.foreground8 c)
  . last color8 (T.background8 c)
  . styleCommon (T.common8 c)

style256 :: T.Style256 -> Gen b -> Gen b
style256 c
  = last color256 (T.foreground256 c)
  . last color256 (T.background256 c)
  . styleCommon (T.common256 c)

textSpec :: T.TextSpec -> Gen b -> Gen b
textSpec c
  = style8 (T.style8 c)
  . style256 (T.style256 c)

chunk :: T.Chunk -> Gen b -> Gen b
chunk c
  = textSpec (T.textSpec c)
  . coarbitraryList text (T.text c)
