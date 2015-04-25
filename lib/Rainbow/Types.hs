{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor,
             DeriveTraversable, DeriveFoldable, TemplateHaskell,
             FlexibleInstances, TypeFamilies,
             MultiParamTypeClasses #-}

-- | The innards of Rainbow.  Ordinarily you should not need this
-- module; instead, just import "Rainbow", which
-- re-exports the most useful names from this module.

module Rainbow.Types where

-- # Imports

import Data.Word (Word8)
import GHC.Generics
import Data.Typeable
import Data.Foldable ()
import Data.Traversable ()
import Control.Lens
import Data.Foldable (Foldable)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as X
import qualified Data.Text.Lazy as XL

--
-- Blank
--

-- | Something is Blank if, upon rendering, it has no visible effect
-- on the terminal.
class Blank a where
  blank :: a

instance Blank String where
  blank = ""

instance Blank BS.ByteString where
  blank = BS.empty

instance Blank BSL.ByteString where
  blank = BSL.empty

instance Blank X.Text where
  blank = X.empty

instance Blank XL.Text where
  blank = XL.empty

--
-- Colors
--

newtype Color a = Color (Maybe a)
  deriving (Eq, Show, Ord, Generic, Typeable, Functor, Foldable,
            Traversable)

makeWrapped ''Color

instance Blank (Color a) where
  blank = Color Nothing

-- | A simple enumeration for eight values.
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

--
-- Styles
--

-- | Style elements that apply in both 8 and 256 color
-- terminals. However, the elements are described separately for 8 and
-- 256 color terminals, so that the text appearance can change
-- depending on how many colors a terminal has.
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

instance Blank Format where
  blank = Format False False False False False False False False

data Style a = Style
  { _fore :: Color a
  , _back :: Color a
  , _format :: Format
  } deriving (Show, Eq, Ord, Generic, Typeable, Functor, Foldable,
              Traversable)

makeLenses ''Style

instance Blank (Style a) where
  blank = Style blank blank blank

--
-- Chunks
--

-- | A chunk is some textual data coupled with a description of what
-- color the text is, attributes like whether it is bold or
-- underlined, etc. The chunk knows what foreground and background
-- colors and what attributes to use for both an 8 color terminal and
-- a 256 color terminal.

data Chunk a = Chunk
  { _style8 :: Style Enum8
  , _style256 :: Style Word8
  , _yarn :: a
  } deriving (Eq, Show, Ord, Generic, Typeable, Functor,
              Foldable, Traversable)

chunk :: a -> Chunk a
chunk = Chunk blank blank

instance Blank a => Blank (Chunk a) where
  blank = Chunk blank blank blank

makeLenses ''Chunk


-- | Stores colors that may affect 8-color terminals, 256-color
-- terminals, both, or neither.
data Radiant = Radiant
  { _color8 :: Color Enum8
  , _color256 :: Color Word8
  } deriving (Eq, Ord, Show)

makeLenses ''Radiant

