module Rainbow.Colors.Shrinkers where

import qualified Rainbow.Colors as C
import qualified Rainbow.Types.Shrinkers as S
import qualified Prelude.Shrinkers

radiant :: C.Radiant -> [C.Radiant]
radiant (C.Radiant c8 mc256) =
  zipWith C.Radiant (S.color8 c8)
    (Prelude.Shrinkers.maybe S.color256 mc256)
