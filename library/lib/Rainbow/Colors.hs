-- | Ordinarily you should not need this module.  Typically you will
-- just need to use "Rainbow.ColorChunks", which is
-- re-exported from "Rainbow".  However this module
-- can be useful if you want names for individual colors, as opposed
-- to names for chunks, which is what
-- "Rainbow.ColorChunks" provides.
module Rainbow.Colors where

import Rainbow.Types
import Data.Monoid
import qualified System.Console.Terminfo as T
import Data.Word (Word8)

-- * 8 color

noColor8 :: Color8
noColor8 = Color8 Nothing

black :: Color8
black = Color8 (Just E0)

red :: Color8
red = Color8 (Just E1)

green :: Color8
green = Color8 (Just E2)

yellow :: Color8
yellow = Color8 (Just E3)

blue :: Color8
blue = Color8 (Just E4)

magenta :: Color8
magenta = Color8 (Just E5)

cyan :: Color8
cyan = Color8 (Just E6)

white :: Color8
white = Color8 (Just E7)

-- * 256 color

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

newtype Both = Both Color8
  deriving (Eq, Ord, Show)

both :: Color8 -> Both
both = Both

data Enum8
  = E0
  | E1
  | E2
  | E3
  | E4
  | E5
  | E6
  | E7
  deriving (Eq, Ord, Show)

enum8toWord8 :: Enum8 -> Word8
enum8toWord8 e = case e of
  E0 -> 0
  E1 -> 1
  E2 -> 2
  E3 -> 3
  E4 -> 4
  E5 -> 5
  E6 -> 6
  E7 -> 7

-- | Color for an 8-color terminal.

newtype Color8 = Color8
  { unColor8 :: Maybe Enum8
  -- ^ Nothing indicates to use the default color for the terminal;
  -- otherwise, use the corresponding Terminfo 'T.Color'.
  } deriving (Eq, Ord, Show)

color8toTerminfo :: Color8 -> Maybe T.Color
color8toTerminfo = fmap (T.ColorNumber . fromIntegral . enum8toWord8)
  . unColor8

-- | Color for an 256-color terminal.

newtype Color256 = Color256
  { unColor256 :: Maybe Word8
  -- ^ Nothing indicates to use the default color for the terminal;
  -- otherwise, use the corresponding Terminfo 'T.Color'.
  } deriving (Eq, Ord, Show)

color256toTerminfo :: Color256 -> Maybe T.Color
color256toTerminfo = fmap (T.ColorNumber . fromIntegral)
  . unColor256

-- | Any color for an 8-color terminal can also be used in a
-- 256-color terminal.
to256 :: Color8 -> Color256
to256 (Color8 mayE) = Color256 $ fmap enum8toWord8 mayE

instance Color Both where
  fore (Both c) = fore c <> fore (to256 c)
  back (Both c) = back c <> back (to256 c)

instance Color Enum8 where
  back = back . Color8 . Just
  fore = fore . Color8 . Just

instance Color Word8 where
  back = back . Color256 . Just
  fore = fore . Color256 . Just
