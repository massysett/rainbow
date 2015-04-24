{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor,
             DeriveTraversable, DeriveFoldable, TemplateHaskell #-}

-- | The innards of Rainbow.  Ordinarily you should not need this
-- module; instead, just import "Rainbow", which
-- re-exports the most useful names from this module.

module Rainbow.Types where

-- # Imports

import qualified Data.String as Str
import Data.Functor
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Text.Lazy as XL
import Data.Word (Word8)
import GHC.Generics
import Data.Typeable
import Data.Foldable ()
import Data.Traversable ()
import Control.Lens
import Data.Foldable (Foldable)

--
-- Colors
--

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

data Style a = Style
  { _fore :: Maybe a
  , _back :: Maybe a
  , _format :: Format
  } deriving (Show, Eq, Ord, Generic, Typeable, Functor, Foldable,
              Traversable)

makeLenses ''Style

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

makeLenses ''Chunk
