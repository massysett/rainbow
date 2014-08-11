-- | Color chunks.
--
-- Names in this module follow these connventions:
--
-- * The prefix @f_@ indicates that a Chunk affects foreground
-- colors.  The prefix @b_@ indicates background colors.
--
-- * The prefix @c8_@ indicates that a Chunk affects 8-color
-- terminals only; @c256_@ indicates 256-color terminals only.  If a
-- Chunk has neither prefix, it affects both 8- and 256-color
-- terminals.
--
-- * Some colors have names.  The color names assume a palette
-- similar to the default one that xterm uses.  Other colors simply
-- have numbers.  Ultimately the interpretation of either named or
-- numbered colors is up to the user's terminal.
module Rainbow.ColorChunks where

import Rainbow.Types
import Rainbow.Colors
import Data.Monoid
import Data.Word (Word8)

class Color a where
  back :: a -> Chunk
  fore :: a -> Chunk

instance Color Color8 where
  back c = Chunk ts []
    where
      ts = TextSpec s8 mempty
      s8 = Style8 mempty b8 mempty
      b8 = Last (Just c)

  fore c = Chunk ts []
    where
      ts = TextSpec s8 mempty
      s8 = Style8 f8 mempty mempty
      f8 = Last (Just c)

instance Color Color256 where
  back c = Chunk ts []
    where
      ts = TextSpec mempty s256
      s256 = Style256 mempty b256 mempty
      b256 = Last (Just c)

  fore c = Chunk ts []
    where
      ts = TextSpec mempty s256
      s256 = Style256 f256 mempty mempty
      f256 = Last (Just c)

newtype Both = Both Color8
  deriving (Eq, Ord, Show)

both :: Color8 -> Both
both = Both

instance Color Both where
  fore (Both c) = fore c <> fore (to256 c)
  back (Both c) = back c <> back (to256 c)

instance Color Enum8 where
  back = back . Color8 . Just
  fore = fore . Color8 . Just

instance Color Word8 where
  back = back . Color256 . Just
  fore = fore . Color256 . Just
