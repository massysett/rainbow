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
-- A 'Chunk' is a 'Data.Monoid.Monoid', so you can combine them using
-- the usual monoid functions, including 'Data.Monoid.<>'. You can
-- create a 'Chunk' with text using 'Data.String.fromString', but this
-- library is much more usable if you enable the OverloadedStrings GHC
-- extension:
--
-- > {-# LANGUAGE OverloadedStrings #-}
--
-- and all future examples assume you have enabled OverloadedStrings.
-- You will also want the Monoid module in scope:
--
-- > import Data.Monoid
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
-- supports 256 colors (like @xterm-256color@) before you start GHCi:
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
-- import Data.Word ('Data.Word.Word8')
-- 'putChunkLn' $ \"Pink on 256-color terminal only\"
--                \<> 'fore' (201 :: 'Data.Word.Word8')
-- @
--
-- If 'mappend' multiple chunks that change the same property, the
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
--                \<> 'fore' 'red' \<> 'fore' (201 :: 'Data.Word.Word8')
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

module Rainbow
  (

  -- * Terminal definitions
    Term(..)
  , termFromEnv
  , smartTermFromEnv

  -- * Chunks
  , Chunk(..)
  , fromText
  , fromLazyText

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
  -- @
  -- 'putChunkLn' $ \"bold on 8 and 256 color terminals\" \<> 'bold'
  -- @

  , bold
  , underline
  , flash
  , inverse

  -- * Effects for 8-color terminals only

  -- | These 'Chunk's affect 8-color terminals only.
  --
  -- @
  -- 'putChunkLn' $ \"Bold on 8 color terminal only\" \<> 'bold8'
  -- @

  , bold8
  , underline8
  , flash8
  , inverse8

  -- * Effects for 256-color terminals only

  -- | These 'Chunk's affect 256-color terminals only.
  --
  -- @
  -- 'putChunkLn' $ \"Underlined on 256-color terminal, \"
  --              \<> \"bold on 8-color terminal\"
  --              \<> 'underline256' \<> 'bold8'
  -- @

  , bold256
  , underline256
  , flash256
  , inverse256

  -- * Colors

  -- ** Changing the foreground and background color
  , Color(..)

  -- ** Colors for both 8- and 256-color terminals
  , Radiant(..)
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
  , grey
  , brightRed
  , brightGreen
  , brightYellow
  , brightBlue
  , brightMagenta
  , brightCyan
  , brightWhite
  , to256

  ) where

import Rainbow.Types
import Rainbow.Colors
