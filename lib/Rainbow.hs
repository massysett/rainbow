{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rainbow handles colors and special effects for text.
--
-- The building block of Rainbow is the 'Chunk'. Each 'Chunk' comes with
-- a 'TextSpec', which specifies how the text should look on 8-color
-- and on 256-color terminals. The 'Chunk' is a full specification; that
-- is, although 'Chunk's are typically printed one after the other, the
-- appearance of one 'Chunk' does not affect the appearance of the next
-- 'Chunk'.
--
-- You have full freedom to specify different attributes and colors
-- for 8 and 256 color terminals; for instance, you can have text
-- appear red on an 8-color terminal but blue on a 256-color terminal.
--
-- A 'Chunk' is a 'Monoid', so you can combine them using
-- the usual monoid functions, including '<>'. You can
-- create a 'Chunk' with text using 'Data.String.fromString', but this
-- library is much more usable if you enable the OverloadedStrings GHC
-- extension:
--
-- > {-# LANGUAGE OverloadedStrings #-}
--
-- or, in GHCi:
--
-- >>> :set -XOverloadedStrings
--
-- and all future examples assume you have enabled OverloadedStrings.
--
-- Here are some basic examples:
--
-- @
-- 'putChunkLn' $ \"Some blue text\" \<> 'fore' 'blue'
-- 'putChunkLn' $ \"Blue on red background\"
--               \<> 'fore' 'blue' \<> 'back' 'red'
-- 'putChunkLn' $ \"Blue on red, foreground bold\"
--                \<> 'fore' 'blue' \<> 'back' 'red' \<> 'bold'
-- @
--
-- But what makes Rainbow a little more interesting is that you can
-- also specify output for 256-color terminals. To use these examples,
-- be sure your TERM environment variable is set to something that
-- supports 256 colors (like @xterm-256color@) before you start GHCi.
--
-- @
-- 'putChunkLn' $ \"Blue on 8-color terminal, red on 256-color terminal\"
--                 \<> 'fore' 'blue8' \<> 'fore' ('to256' 'red8')
-- @
--
-- To get a 'Color256', which affects only 256-color terminals, there
-- are some definitions in the module such as 'brightRed'.  You may
-- also use 'Word8' literals, like this.  You need to specify the type
-- as it can't be inferred:
--
-- @
-- 'putChunkLn' $ \"Pink on 256-color terminal only\"
--                \<> 'fore' (201 :: 'Word8')
-- @
--
-- If you 'mappend' multiple chunks that change the same property, the
-- rightmost one \"wins\":
--
-- @
-- 'putChunkLn' $ \"This will be blue\" \<> 'fore' 'red' \<> 'fore' 'blue'
-- @
--
-- This property comes in handy if you want to specify a default color
-- for 8- and 256-color terminals, then a more specific shade for a
-- 256-color terminal:
--
-- @
-- 'putChunkLn' $ \"Red on 8-color, pink on 256-color\"
--                \<> 'fore' 'red' \<> 'fore' (201 :: 'Word8')
-- @
--
-- However, if you use 'mappend' to add additional 'Chunk's that have
-- text, the text will be appended:
--
-- @
-- 'putChunkLn' $ 'fore' 'green' \<> \"You will see this text \"
--              \<> \"and this text too, but it will all be blue\"
--              \<> 'fore' 'blue'
-- @
--
-- Although one chunk can have different colors on 8- and 256-color
-- terminals, it cannot have different colors on the same
-- terminal. That is, if you want to print some text in one color and
-- some text in another color, make two chunks.


module Rainbow where

-- # Imports

import qualified Data.String as Str
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import Data.List (intersperse)

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

-- | The Scheme bundles together the styles for the 8 and 256 color
-- terminals, so that the text can be portrayed on any terminal.
data Scheme = Scheme
  { _c8 :: Style Enum8
  , _c256 :: Style Word8
  } deriving (Show, Eq, Ord, Generic, Typeable)

makeLenses ''Scheme

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

makeLenses ''Chunk

class Renderable a where
  render :: a -> [ByteString] -> [ByteString]

instance Renderable Char where
  render c = ((BS8.singleton c) :)

escape :: [ByteString] -> [ByteString]
escape = render '\x1B'

csi :: [ByteString] -> [ByteString]
csi = escape . render '['

sgr :: ([ByteString] -> [ByteString]) -> [ByteString] -> [ByteString]
sgr sq = csi . sq . render 'm'

params :: Show a => [a] -> [ByteString] -> [ByteString]
params cs = ((intersperse ";" . map (BS8.pack . show) $ cs) ++)

sgrSingle :: Word -> [ByteString] -> [ByteString]
sgrSingle w = sgr $ params [w]

sgrDouble :: Word -> Word -> [ByteString] -> [ByteString]
sgrDouble x y = sgr $ params [x, y]

normalDefault :: [ByteString] -> [ByteString]
normalDefault = sgrSingle 0



{-
  (

  -- * Chunks
    Chunk
  , chunkFromText
  , chunkFromTexts
  , chunkFromLazyText
  , chunkFromLazyTexts

  -- * Effects for both 8 and 256 color terminals

  -- | These 'Chunk's affect both 8 and 256 color terminals:
  --
  -- @
  -- 'putChunkLn' $ \"bold on 8 and 256 color terminals\" \<> 'bold'
  -- @

  , bold
  , faint
  , italic
  , underline
  , blink
  , inverse
  , invisible
  , strikeout

  -- * Effects for 8-color terminals only

  -- | These 'Chunk's affect 8-color terminals only.
  --
  -- @
  -- 'putChunkLn' $ \"Bold on 8 color terminal only\" \<> 'bold8'
  -- @

  , bold8
  , faint8
  , italic8
  , underline8
  , blink8
  , inverse8
  , invisible8
  , strikeout8

  -- * Effects for 256-color terminals only

  -- | These 'Chunk's affect 256-color terminals only.
  --
  -- @
  -- 'putChunkLn' $ \"Underlined on 256-color terminal, \"
  --              \<> \"bold on 8-color terminal\"
  --              \<> 'underline256' \<> 'bold8'
  -- @

  , bold256
  , faint256
  , italic256
  , underline256
  , blink256
  , inverse256
  , invisible256
  , strikeout256

  -- * Colors

  -- ** Changing the foreground and background color
  , Color(..)

  -- ** Colors for both 8- and 256-color terminals
  , Radiant(..)
  , noColorRadiant
  , both
  , black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white


  -- ** Colors for 8-color terminals only
  , Enum8(..)
  , Color8(..)
  , noColor8
  , black8
  , red8
  , green8
  , yellow8
  , blue8
  , magenta8
  , cyan8
  , white8

  -- ** Colors for 256-color terminals only
  , Color256(..)
  , noColor256
  , grey
  , brightRed
  , brightGreen
  , brightYellow
  , brightBlue
  , brightMagenta
  , brightCyan
  , brightWhite
  , to256

  -- * Converting 'Chunk' to 'Data.ByteString.ByteString'

  -- | To print a 'Chunk', you need to convert it to some
  -- 'Data.ByteString.ByteString's.
  --
  -- All these functions convert the 'Data.Text' to UTF-8
  -- 'Data.ByteString.ByteString's.  Many of these functions return a
  -- difference list.  Learn You a Haskell for Great Good has a great
  -- explanation of difference lists:
  --
  -- http://learnyouahaskell.com/for-a-few-monads-more
  --
  -- If you don't want to learn about difference lists, just stick
  -- with using 'chunksToByteStrings' and use
  -- 'byteStringMakerFromEnvironment' if you want to use the highest
  -- number of colors possible, or, to manually specify the number of
  -- colors, use 'chunksToByteStrings' with 'toByteStringsColors0',
  -- 'toByteStringsColors8', or 'toByteStringsColors256' as the first
  -- argument.  'chunksToByteStrings' has an example.
  , byteStringMakerFromEnvironment
  , byteStringMakerFromHandle
  , toByteStringsColors0
  , toByteStringsColors8
  , toByteStringsColors256
  , chunksToByteStrings

  -- * Quick and dirty functions for IO

  -- | For efficiency reasons you probably don't want to use these
  -- when printing large numbers of 'Chunk', but they are handy for
  -- throwaway uses like experimenting in GHCi.
  , putChunk
  , putChunkLn

  -- * Re-exports
  -- $reexports
  -- | * "Data.Monoid" re-exports '<>' and 'mempty'
  --
  -- 
  , module Data.Monoid
  , module Data.ByteString
  , module Data.Text
  , module Data.Word

  -- * Notes on terminals
  -- $termNotes

  ) where

import Rainbow.Types
import Rainbow.Colors
import Rainbow.Translate
  ( byteStringMakerFromEnvironment
  , byteStringMakerFromHandle
  , toByteStringsColors0
  , toByteStringsColors8
  , toByteStringsColors256
  , chunksToByteStrings
  , putChunk
  , putChunkLn
  )
import Data.Monoid (Monoid, (<>), mempty)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Word (Word8)

{- $reexports

   * "Data.Monoid" re-exports 'Monoid', '<>' and 'mempty'

   * "Data.ByteString" re-exports 'ByteString'

   * "Data.Text" re-exports 'Text'

   * "Data.Word" re-exports 'Word8'
-}

{- $termNotes

Earlier versions of Rainbow used the Haskell terminfo library for
dealing with the terminal.  Terminfo is available at

<https://hackage.haskell.org/package/terminfo>

Terminfo, in turn, uses the UNIX terminfo library.  The biggest
advantage of using Terminfo is that it is compatible with a huge
variety of terminals.  Many of these terminals are hardware models
that are gathering dust in an IBM warehouse somewhere, but even modern
software terminals might have quirks.  Terminfo covers all those.

The disadvantage is that using Terminfo requires you to perform IO
whenever you need to format output for the terminal.  Your only choice
when using Terminfo is to send output directly to the terminal, or to
a handle.  This goes against typical Haskell practice, where we try to
write pure code whenever possible.

Perhaps surprisingly, there are times where you may want to format
output, but not immediately send it to the terminal.  Maybe you want
to send it to a file instead, or maybe you want to use a Haskell
library like Pipes and stream it somewhere.  Terminfo is a binding to
a Unix library that is not designed for this sort of thing.  The
closest you could get using Terminfo would be to make a Handle that is
backed by a in-memory buffer.  There is a package for that sort of
thing:

<http://hackage.haskell.org/package/knob>

but it seems like a nasty workaround.  Or you can hijack stdout and
send that somewhere--again, nasty workaround.

So I decided to stop using Terminfo.  That means Rainbow no longer
supports a menagerie of bizarre terminals.  It instead just uses the
standard ISO 6429 / ECMA 48 terminal codes.  These are the same codes
that are used by xterm, the OS X Terminal, the Linux console, or any
other reasonably modern software terminal.  Realistically they are the
only terminals Rainbow would be used for.

The 256 color capability is not in ISO 6429, but it is widely supported.

Probably the most common so-called terminals in use today that do NOT
support the ISO 6429 codes are those that are not really terminals.
For instance, you might use an Emacs shell buffer.  For those
situations just use 'toByteStringsColors0'.

I also decided to standardize on UTF-8 for the 'Text' output..  These
days that seems reasonable.

Now, to figure out how many colors the terminal supports, Rainbow
simply uses the @tput@ program.  This removes the dependency on
Terminfo altogether.

Apparently it's difficult to get ISO 6429 support on Microsoft
Windows.  Oh well.

-}
-}
