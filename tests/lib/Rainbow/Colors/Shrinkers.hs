module Rainbow.Colors.Shrinkers where

import qualified Rainbow.Colors as C
import qualified Rainbow.Types.Shrinkers as S

both :: C.Both -> [C.Both]
both (C.Both b) = map C.Both $ S.color8 b
