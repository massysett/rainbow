module Rainbow.Colors.Shrinkers where

import qualified Rainbow.Colors as C
import qualified Rainbow.Types.Shrinkers as S
import qualified Prelude.Shrinkers

both :: C.Both -> [C.Both]
both (C.Both c8 mc256) =
  zipWith C.Both (S.color8 c8)
    (Prelude.Shrinkers.maybe S.color256 mc256)
