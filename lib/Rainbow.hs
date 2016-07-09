{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Rainbow handles colors and special effects for text.  The basic
-- building block of Rainbow is the 'Y.Chunk'.  The 'Y.Chunk' contains
-- both text and formatting information such as colors, bold,
-- underlining, etc.
--
-- When printed, each 'Y.Chunk' starts off with a clean slate, so if
-- you want special formatting such as any color, bold, etc, then you
-- must specify it for every 'Y.Chunk'.  The appearance of one
-- 'Y.Chunk' does not affect the appearance of the next 'Y.Chunk'.
-- This makes it easy to reason about how a particular 'Y.Chunk' will
-- look.
--
-- Rainbow supports 256-color terminals.  You have full freedom to
-- specify different attributes and colors for 8 and 256 color
-- terminals; for instance, you can have text appear red on an 8-color
-- terminal but blue on a 256-color terminal.
--
-- Here are some basic examples:
--
-- @
-- 'T.putChunkLn' $ 'Y.chunk' \"Some blue text\" '&' 'fore' 'blue'
-- 'T.putChunkLn' $ 'Y.chunk' \"Blue on red background\"
--               '&' 'fore' 'blue' '&' 'back' 'red'
-- 'T.putChunkLn' $ 'Y.chunk' \"Blue on red, foreground bold\"
--                '&' 'fore' 'blue' '&' 'back' 'red' '&' 'bold'
-- @
--
-- You can also specify output for 256-color terminals. To use these
-- examples, be sure your TERM environment variable is set to
-- something that supports 256 colors (like @xterm-256color@) before
-- you start GHCi.
--
-- @
-- 'T.putChunkLn' $ 'Y.chunk' \"Blue on 8, bright green on 256\" '&'
--    'fore' ('blue' '<>' 'brightGreen')
--
-- 'T.putChunkLn' $ 'Y.chunk' \"Blue on 8, red on 256" '&'
--    'fore' ('blue' '<>' 'only256' 'red')
-- @
--
-- Each 'Y.Chunk' affects the formatting only of that 'Y.Chunk'.  So
-- to print things in different colors, make more than one 'Y.Chunk':
--
-- @
-- 'mapM_' 'T.putChunkLn'
--    [ 'Y.chunk' \"Roses\" '&' 'fore' 'red'
--    , 'Y.chunk' \"Violets\" '&' 'fore' 'blue' ]
-- @
--
-- The above examples use 'T.putChunkLn', but that function will
-- be inefficient if you are printing many 'Y.Chunk's.  For
-- greater efficiency see 'T.chunksToByteStrings'.
--
-- The functions in this module, "Rainbow", will likely be enough for
-- most uses, but for more flexibility you can use "Rainbow.Types".
-- Use of "Rainbow.Types" will require some familiarity with the
-- @lens@ library.


module Rainbow
  (
  -- * Chunk
    Y.Chunk
  , Y.chunk

  -- * Formatting, all terminals

  -- | These combinators affect the way a 'Y.Chunk' is displayed on
  -- both 8- and 256-color terminals.
  , bold
  , faint
  , italic
  , underline
  , blink
  , inverse
  , invisible
  , strikeout

  -- * Colors
  , Y.Radiant
  , fore
  , back

  -- * Colors, all terminals

  -- | These 'Y.Radiant' affect the way a 'Y.Chunk' is displayed on
  -- both 8- and 256-color terminals.
  , black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white

  -- * Colors, 256-color terminals only

  -- | These 'Y.Radiant' affect 256-color terminals only.
  , grey
  , brightRed
  , brightGreen
  , brightYellow
  , brightBlue
  , brightMagenta
  , brightCyan
  , brightWhite
  , color256
  , only256

  -- * Converting 'Y.Chunk' to 'Data.ByteString.ByteString'

  -- | To print a 'Y.Chunk', you need to convert it to some
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
  -- with using 'T.chunksToByteStrings' and use
  -- 'T.byteStringMakerFromEnvironment' if you want to use the highest
  -- number of colors possible, or, to manually specify the number of
  -- colors, use 'T.chunksToByteStrings' with 'T.toByteStringsColors0',
  -- 'T.toByteStringsColors8', or 'T.toByteStringsColors256' as the first
  -- argument.  'T.chunksToByteStrings' has an example.
  , T.Renderable
  , T.toByteStringsColors0
  , T.toByteStringsColors8
  , T.toByteStringsColors256
  , T.byteStringMakerFromEnvironment
  , T.byteStringMakerFromHandle
  , T.chunksToByteStrings

  -- * Quick and dirty functions for IO

  -- | For efficiency reasons you probably don't want to use these
  -- when printing large numbers of 'Chunk', but they are handy for
  -- throwaway uses like experimenting in GHCi.
  , T.putChunk
  , T.putChunkLn

  -- * Re-exports
  -- $reexports
  , module Data.Function
  , module Data.Word
  , module Data.ByteString
  , module Data.Monoid

  -- * Notes on terminals
  -- $termNotes

  ) where

import qualified Rainbow.Translate as T
import qualified Rainbow.Types as Y
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Function ((&))
import qualified Lens.Simple as Lens
import Lens.Simple ((.~))
import Data.Monoid (Monoid(mempty), (<>))

formatBoth :: Lens.Setter' Y.Format Bool -> Y.Chunk a -> Y.Chunk a
formatBoth get c = c & Y.scheme . Y.style8 . Y.format . get .~ True
  & Y.scheme . Y.style256 . Y.format . get .~ True

-- | Bold. What actually happens when you use Bold is going to depend
-- on your terminal. For example, xterm allows you actually use a bold
-- font for bold, if you have one. Otherwise, it might simulate bold
-- by using overstriking. Another possibility is that your terminal
-- might use a different color to indicate bold. For more details (at
-- least for xterm), look at xterm (1) and search for @boldColors@.
--
-- If your terminal uses a different color for bold, this allows an
-- 8-color terminal to really have 16 colors.
bold :: Y.Chunk a -> Y.Chunk a
bold = formatBoth Y.bold

faint :: Y.Chunk a -> Y.Chunk a
faint = formatBoth Y.faint

italic :: Y.Chunk a -> Y.Chunk a
italic = formatBoth Y.italic

underline :: Y.Chunk a -> Y.Chunk a
underline = formatBoth Y.underline

blink :: Y.Chunk a -> Y.Chunk a
blink = formatBoth Y.blink

inverse :: Y.Chunk a -> Y.Chunk a
inverse = formatBoth Y.inverse

invisible :: Y.Chunk a -> Y.Chunk a
invisible = formatBoth Y.invisible

strikeout :: Y.Chunk a -> Y.Chunk a
strikeout = formatBoth Y.strikeout

-- | Change the foreground color for both 8- and 256-color terminals.
fore :: Y.Radiant -> Y.Chunk a -> Y.Chunk a
fore (Y.Radiant c8 c256) c = c & Y.scheme . Y.style8 . Y.fore .~ c8
  & Y.scheme . Y.style256 . Y.fore .~ c256

-- | Change the background color for both 8- and 256-color terminals.
back :: Y.Radiant -> Y.Chunk a -> Y.Chunk a
back (Y.Radiant c8 c256) c = c & Y.scheme . Y.style8 . Y.back .~ c8
  & Y.scheme . Y.style256 . Y.back .~ c256

-- | Ensures that a 'Y.Radiant' affects only a 256-color terminal.
-- For instance, to make text that is blue on an 8-color terminal but
-- red on a 256-color terminal:
--
-- @
-- 'T.putChunkLn' $ 'chunk' \"Blue on 8, red on 256" &
--    'fore' ('blue' <> 'only256' 'red')
-- @
only256 :: Y.Radiant -> Y.Radiant
only256 (Y.Radiant _ c256) = Y.Radiant mempty c256

black :: Y.Radiant
black = Y.Radiant (Y.Color (Just Y.E0)) (Y.Color (Just 0))

red :: Y.Radiant
red = Y.Radiant (Y.Color (Just Y.E1)) (Y.Color (Just 1))

green :: Y.Radiant
green = Y.Radiant (Y.Color (Just Y.E2)) (Y.Color (Just 2))

yellow :: Y.Radiant
yellow = Y.Radiant (Y.Color (Just Y.E3)) (Y.Color (Just 3))

blue :: Y.Radiant
blue = Y.Radiant (Y.Color (Just Y.E4)) (Y.Color (Just 4))

magenta :: Y.Radiant
magenta = Y.Radiant (Y.Color (Just Y.E5)) (Y.Color (Just 5))

cyan :: Y.Radiant
cyan = Y.Radiant (Y.Color (Just Y.E6)) (Y.Color (Just 6))

white :: Y.Radiant
white = Y.Radiant (Y.Color (Just Y.E7)) (Y.Color (Just 7))

grey :: Y.Radiant
grey = color256 8

brightRed :: Y.Radiant
brightRed = color256 9

brightGreen :: Y.Radiant
brightGreen = color256 10

brightYellow :: Y.Radiant
brightYellow = color256 11

brightBlue :: Y.Radiant
brightBlue = color256 12

brightMagenta :: Y.Radiant
brightMagenta = color256 13

brightCyan :: Y.Radiant
brightCyan = color256 14

brightWhite :: Y.Radiant
brightWhite = color256 15

color256 :: Word8 -> Y.Radiant
color256 x = Y.Radiant (Y.Color Nothing) (Y.Color (Just x))


{- $reexports

   * "Data.Function" re-exports '&'

   * "Data.Monoid" re-exports 'Monoid', '<>' and 'mempty'

   * "Data.ByteString" re-exports 'ByteString'

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

I also decided to standardize on UTF-8 for the 'Text' output.  These
days that seems reasonable.

Now, to figure out how many colors the terminal supports, Rainbow
simply uses the @tput@ program.  This removes the dependency on
Terminfo altogether.

Apparently it's difficult to get ISO 6429 support on Microsoft
Windows.  Oh well.

-}
