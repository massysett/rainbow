module Rainbow.Types.Coarbitrary where

import Test.QuickCheck
import Data.Monoid.Coarbitrary
import Data.Text.Coarbitrary
import qualified Rainbow.Types as T
import Prelude hiding (last, maybe)
import Barecheck.Util
import Prelude.Coarbitrary

enum8 :: T.Enum8 -> Gen b -> Gen b
enum8 e = case e of
  T.E0 -> varInt 0
  T.E1 -> varInt 1
  T.E2 -> varInt 2
  T.E3 -> varInt 3
  T.E4 -> varInt 4
  T.E5 -> varInt 5
  T.E6 -> varInt 6
  T.E7 -> varInt 7

color8 :: T.Color8 -> Gen b -> Gen b
color8 (T.Color8 me) = maybe enum8 me

color256 :: T.Color256 -> Gen b -> Gen b
color256 (T.Color256 me) = maybe variant me

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
  . list text (T.text c)
