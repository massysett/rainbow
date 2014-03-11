-- | Handles colors and special effects for text. Internally this
-- module uses the Haskell terminfo library, which links against the
-- UNIX library of the same name, so it should work with a wide
-- variety of UNIX terminals.
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
-- A 'Chunk' is a 'Monoid', so you can combine them using the usual
-- monoid functions, including '<>'. You can create a 'Chunk' with
-- text using 'fromString', but this library is much more usable if
-- you enable the OverloadedStrings GHC extension:
--
-- > {-# LANGUAGE OverloadedStrings #-}
--
-- and all future examples assume you have enabled OverloadedStrings.
--
-- Here are some basic examples:
--
-- > putChunkLn $ "Some blue text" <> f_blue
-- > putChunkLn $ "Blue on red background" <> f_blue <> b_red
-- > putChunkLn $ "Blue on red, foreground bold" <> f_blue <> b_red <> bold
--
-- But what makes Rainbow a little more interesting is that you can
-- also specify output for 256-color terminals. To use these examples,
-- be sure your TERM environment variable is set to something that
-- supports 256 colors (like @xterm-256color@) before you start GHCi:
--
-- > putChunkLn $ "Blue on 8-color terminal, red on 256-color terminal"
-- >            <> c8_f_blue <> c256_f_red
--
-- If 'mappend' multiple chunks that change the same property, the
-- rightmost one \"wins\":
--
-- > putChunkLn $ "This will be blue" <> f_red <> f_blue
--
-- This property comes in handy if you want to specify a default color
-- for 8- and 256-color terminals, then a more specific shade for a
-- 256-color terminal:
--
-- > putChunkLn $ "Pink" <> f_red <> c256_f_201
--
-- However, if you use 'mappend' to add additional 'Chunk's that have
-- text, the text will be appended:
--
-- > putChunkLn $ f_green <> "You will see this text "
-- >              <> "and this text too, but it will all be blue"
-- >              <> f_blue
--
-- Although one chunk can have different colors on 8- and 256-color
-- terminals, it cannot have different colors on the same
-- terminal. That is, if you want to print some text in one color and
-- some text in another color, make two chunks.

module System.Console.Rainbow.Lazy
  (

  -- * Terminal definitions
    Term(..)
  , termFromEnv
  , smartTermFromEnv

  -- * Chunks
  , Chunk(..)
  , fromText

  -- * Printing chunks
  , putChunks
  , hPutChunks

  -- ** Printing one chunk at a time

  -- | These functions make it easy to print one chunk at a time. Each
  -- function initializes the terminal once for each chunk, unlike the
  -- list-oriented functions, which only initialize the terminal
  -- once. (Initialization does not clear the screen; rather, it is a
  -- process that the terminfo library requires.) Thus there might be
  -- a performance penalty for using these functions to print large
  -- numbers of chunks. Or, there might not be--I have not benchmarked
  -- them.
  --
  -- These functions use the default terminal, obtained using
  -- 'termFromEnv'.
  , putChunk
  , putChunkLn
  , hPutChunk
  , hPutChunkLn

  -- * Effects for both 8 and 256 color terminals

  -- | These 'Chunk's affect both 8 and 256 color terminals:
  --
  -- > putChunkLn $ "bold on 8 and 256 color terminals" <> bold
  --
  -- There are also 'Chunk's to turn an effect off, such as
  -- 'boldOff'. Ordinarily you will not need these because each chunk
  -- starts with no effects, so you only need to turn on the effects
  -- you want. However the @off@ 'Chunk's are here if you need them.

  , bold, boldOff
  , underline, underlineOff
  , flash, flashOff
  , inverse, inverseOff

  -- * Effects for 8-color terminals only

  -- | These 'Chunk's affect 8-color terminals only.
  --
  -- > putChunkLn $ "Bold on 8 color terminal only" <> bold8

  , bold8, bold8off
  , underline8, underline8off
  , flash8, flash8off
  , inverse8, inverse8off

  -- * Effects for 256-color terminals only

  -- | These 'Chunk's affect 256-color terminals only.
  --
  -- > putChunkLn $ "Underlined on 256-color terminal, "
  -- >              <> "bold on 8-color terminal"
  -- >              <> underlined256 <> bold8

  , bold256, bold256off
  , underline256, underline256off
  , flash256, flash256off
  , inverse256, inverse256off

  -- * Color chunks
  -- | All colors are within this module.
  , module System.Console.Rainbow.Lazy.ColorChunks

  ) where

import System.Console.Rainbow.Lazy.Types
import System.Console.Rainbow.Lazy.ColorChunks
