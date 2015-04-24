{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Rainbow handles colors and special effects for text.

module Rainbow
  ( Y.Blank(..)

  -- * Colors
  , Y.Color(..)

  -- * Format
  , Y.Format

  -- * Style
  , Y.Style

  -- * Chunk
  , Y.Chunk

  -- * Formatting, all terminals
  , bold
  , faint
  , italic
  , underline
  , blink
  , inverse
  , invisible
  , strikeout

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
  , T.Renderable(..)
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
  ) where

import qualified Rainbow.Translate as T
import qualified Rainbow.Types as Y
import Control.Lens

formatBoth :: Setter' Y.Format Bool -> Y.Chunk a -> Y.Chunk a
formatBoth get c = c & Y.style8 . Y.format . get .~ True
  & Y.style256 . Y.format . get .~ True

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

  , byteStringMakerFromEnvironment
  , byteStringMakerFromHandle
  , toByteStringsColors0
  , toByteStringsColors8
  , toByteStringsColors256
  , chunksToByteStrings

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

I also decided to standardize on UTF-8 for the 'Text' output.  These
days that seems reasonable.

Now, to figure out how many colors the terminal supports, Rainbow
simply uses the @tput@ program.  This removes the dependency on
Terminfo altogether.

Apparently it's difficult to get ISO 6429 support on Microsoft
Windows.  Oh well.

-}
-}
