-- | Ordinarily you should not need this module; typically you will
-- just import "Rainbow", which re-exports the most useful things from
-- this module.  This module also contains data constructors that
-- "Rainbow" does not re-export.
module Rainbow.Colors where

import Data.Maybe (fromMaybe)
import Rainbow.Types
import Data.Monoid
import Data.Word (Word8)

-- * 8 color

-- | Resets the color (foreground or background, as appropriate) to
-- the default for your terminal.  Usually you will not need this, as
-- each 'Chunk' starts out with the terminal's default colors.
noColor8 :: Color8
noColor8 = Color8 Nothing

black8 :: Color8
black8 = Color8 (Just E0)

red8 :: Color8
red8 = Color8 (Just E1)

green8 :: Color8
green8 = Color8 (Just E2)

yellow8 :: Color8
yellow8 = Color8 (Just E3)

blue8 :: Color8
blue8 = Color8 (Just E4)

magenta8 :: Color8
magenta8 = Color8 (Just E5)

cyan8 :: Color8
cyan8 = Color8 (Just E6)

white8 :: Color8
white8 = Color8 (Just E7)

-- * 256 color

-- | Resets the color (foreground or background, as appropriate) to
-- the default for your terminal.  Usually you will not need this, as
-- each 'Chunk' starts out with the terminal's default colors.
noColor256 :: Color256
noColor256 = Color256 Nothing

grey :: Color256
grey = Color256 (Just 8)

brightRed :: Color256
brightRed = Color256 (Just 9)

brightGreen :: Color256
brightGreen = Color256 (Just 10)

brightYellow :: Color256
brightYellow = Color256 (Just 11)

brightBlue :: Color256
brightBlue = Color256 (Just 12)

brightMagenta :: Color256
brightMagenta = Color256 (Just 13)

brightCyan :: Color256
brightCyan = Color256 (Just 14)

brightWhite :: Color256
brightWhite = Color256 (Just 15)

-- * Both 8- and 256-color terminals

-- | A 'Radiant' affects both 8- and 256-color terminals.  (It does
-- /not/ necessarily affect both the foreground and background;
-- whether it affects the foreground, background, or both depends upon
-- the context in which it is used.)
data Radiant = Radiant
  { rad8 :: Color8
  , rad256 :: Maybe Color256
  -- ^ If 'Nothing', use the 'rad8' color on 256-color terminals.
  } deriving (Eq, Ord, Show)

-- | A Radiant with the same color for both 8- and 256-color
-- terminals.
both :: Color8 -> Radiant
both c8 = Radiant c8 Nothing

-- | A Radiant that uses the terminal's default colors for both 8- and
-- 256-color terminals.
noColorRadiant :: Radiant
noColorRadiant = both noColor8

black :: Radiant
black = both black8

red :: Radiant
red = both red8

green :: Radiant
green = both green8

yellow :: Radiant
yellow = both yellow8

blue :: Radiant
blue = both blue8

magenta :: Radiant
magenta = both magenta8

cyan :: Radiant
cyan = both cyan8

white :: Radiant
white = both white8

-- | Changing colors.  Instances of this class affect the background
-- or the foreground color.  For example, to get a 'Chunk' that
-- changes the background to red, use @'back' 'red'@; for the
-- foreground, use @'fore' 'red'@.  Whether 8-color or 256-color
-- terminals (or both) are affected depends on the particular
-- instance.
--
-- Because 'Word8' is an instance of 'Color', you can use literals to
-- affect the color of 256-color terminals.  For example, if you have
-- a 256 color terminal:
--
-- > putChunkLn $ "muddy yellow background" <> back (100 :: Word8)
--
-- This example would not affect an 8-color terminal, as the 'Word8'
-- instance affects 256-color terminals only.

class Color a where
  -- | Create a 'Chunk' that affects the background color only.
  back :: a -> Chunk

  -- | Create a 'Chunk' that affects the foreground color only.
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

-- | Affects the foreground and background of both 8- and 256-color
-- terminals.
instance Color Radiant where
  fore (Radiant c8 mc256) = fore c8
    <> fore (fromMaybe (to256 c8) mc256)
  back (Radiant c8 mc256) = back c8
    <> back (fromMaybe (to256 c8) mc256)

-- | Affects the foreground and background of 8-color terminals.
instance Color Enum8 where
  back = back . Color8 . Just
  fore = fore . Color8 . Just

-- | Affects the foreground and background of 256-color terminals.
instance Color Word8 where
  back = back . Color256 . Just
  fore = fore . Color256 . Just
