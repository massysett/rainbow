{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor,
             DeriveTraversable, DeriveFoldable, TemplateHaskell #-}
-- Lens.Simple makeLenses will not create signatures
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | All the main types in Rainbow.  Using this module you can specify
-- that you want different formatting for 8- and 256-color terminals.
-- Many of the names in this module conflict with the names in
-- "Rainbow", so it's probably best to @import@ this module
-- @qualified@.
module Rainbow.Types where

-- # Imports

import Lens.Simple (makeLenses)
import Data.Traversable ()
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)

--
-- Colors
--

-- | A color; a 'Nothing' value means that the terminal's default
-- color is used.  The type of the 'Maybe' generally will be an
-- 'Enum8' to represent one of 8 colors, or a 'Word8' to represent one
-- of 256 colors.
newtype Color a = Color (Maybe a)
  deriving (Eq, Show, Ord, Generic, Typeable, Functor, Foldable,
            Traversable)

instance Semigroup (Color a) where
  Color x <> Color y = case y of
    Just a -> Color (Just a)
    _ -> Color x

-- | Takes the last non-Nothing Color.  'mempty' is no color.
instance Monoid (Color a) where
  mempty = Color Nothing

-- | A simple enumeration for eight values.  Represents eight colors.
data Enum8
  = E0
  | E1
  | E2
  | E3
  | E4
  | E5
  | E6
  | E7
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, Typeable)

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

black :: Enum8
black = E0

red :: Enum8
red = E1

green :: Enum8
green = E2

yellow :: Enum8
yellow = E3

blue :: Enum8
blue = E4

magenta :: Enum8
magenta = E5

cyan :: Enum8
cyan = E6

white :: Enum8
white = E7

grey :: Word8
grey = 8

brightRed :: Word8
brightRed = 9

brightGreen :: Word8
brightGreen = 10

brightYellow :: Word8
brightYellow = 11

brightBlue :: Word8
brightBlue = 12

brightMagenta :: Word8
brightMagenta = 13

brightCyan :: Word8
brightCyan = 14

brightWhite :: Word8
brightWhite = 15

--
-- Styles
--

-- | Text formatting such as bold, italic, etc.
data Format = Format
  { _bold :: Bool
  , _faint :: Bool
  , _italic :: Bool
  , _underline :: Bool
  , _blink :: Bool
  , _inverse :: Bool
  , _invisible :: Bool
  , _strikeout :: Bool
  } deriving (Show, Eq, Ord, Generic, Typeable)

makeLenses ''Format

instance Semigroup Format where
  (Format x0 x1 x2 x3 x4 x5 x6 x7) <> (Format y0 y1 y2 y3 y4 y5 y6 y7)
    = Format (x0 || y0) (x1 || y1) (x2 || y2) (x3 || y3) (x4 || y4)
             (x5 || y5) (x6 || y6) (x7 || y7)

-- | For each field, the resulting field is True if either field is
-- True.  For 'mempty', every field is False.
instance Monoid Format where
  mempty = Format False False False False False False False False

-- | The foreground and background color, and the 'Format'.  This
-- represents all colors and formatting attributes for either an 8- or
-- 256-color terminal.
data Style a = Style
  { _fore :: Color a
  , _back :: Color a
  , _format :: Format
  } deriving (Show, Eq, Ord, Generic, Typeable, Functor, Foldable,
              Traversable)

makeLenses ''Style

instance Semigroup (Style a) where
  (Style x0 x1 x2) <> (Style y0 y1 y2)
    = Style (x0 <> y0) (x1 <> y1) (x2 <> y2)

-- | Uses the underlying 'Monoid' instances for 'Color' and 'Format'.
instance Monoid (Style a) where
  mempty = Style mempty mempty mempty

--
-- Scheme
--

-- | Holds the 'Style' for both 8- and 256-color terminals.
data Scheme = Scheme
  { _style8 :: Style Enum8
  , _style256 :: Style Word8
  } deriving (Eq, Ord, Show, Generic, Typeable)

makeLenses ''Scheme

instance Semigroup Scheme where
  (Scheme x0 x1) <> (Scheme y0 y1) = Scheme (x0 <> y0) (x1 <> y1)

instance Monoid Scheme where
  mempty = Scheme mempty mempty

--
-- Chunks
--

-- | A chunk is some textual data coupled with a description of what
-- color the text is, attributes like whether it is bold or
-- underlined, etc. The chunk knows what foreground and background
-- colors and what attributes to use for both an 8 color terminal and
-- a 256 color terminal.

data Chunk a = Chunk
  { _scheme :: Scheme
  , _yarn :: a
  } deriving (Eq, Show, Ord, Generic, Typeable, Functor,
              Foldable, Traversable)

instance Semigroup a => Semigroup (Chunk a) where
  (Chunk x0 x1) <> (Chunk y0 y1)
    = Chunk (x0 <> y0) (x1 <> y1)

-- | Uses the underlying 'Monoid' instances for the 'Style' and for
-- the particular '_yarn'.  Therefore 'mempty' will have no formatting
-- and no colors and will generally have no text, though whether or
-- not there is any text depends on the 'mempty' for the type of the
-- '_yarn'.
instance Monoid a => Monoid (Chunk a) where
  mempty = Chunk mempty mempty

-- | Creates a 'Chunk' with no formatting and with the given text.
chunk :: a -> Chunk a
chunk = Chunk mempty

makeLenses ''Chunk


-- | Stores colors that may affect 8-color terminals, 256-color
-- terminals, both, or neither.
data Radiant = Radiant
  { _color8 :: Color Enum8
  , _color256 :: Color Word8
  } deriving (Eq, Ord, Show, Typeable, Generic)

instance Semigroup Radiant where
  (Radiant x0 x1) <> (Radiant y0 y1) = Radiant (x0 <> y0) (x1 <> y1)

-- | Uses the underlying 'Monoid' instance for the 'Color's.  Thus the
-- last non-'Nothing' 'Color' is used.  This can be useful to specify
-- one color for 8-color terminals and a different color for 256-color
-- terminals.
instance Monoid Radiant where
  mempty = Radiant mempty mempty

makeLenses ''Radiant

