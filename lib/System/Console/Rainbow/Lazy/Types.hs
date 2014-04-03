-- | The innards of Rainbow.  Ordinarily you should not need this
-- module; instead, just import "System.Console.Rainbow", which
-- re-exports the most useful names from this module.

module System.Console.Rainbow.Lazy.Types
       ( module System.Console.Rainbow.Lazy.Types
       , module System.Console.Rainbow.Types
       ) where

-- # Imports

import qualified Data.String as Str
import Data.Monoid
import Data.Text.Lazy (Text)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as X
import qualified System.Console.Terminfo as T
import System.IO as IO
import System.Environment as Env
import System.Console.Rainbow.Colors
import System.Console.Rainbow.Types (
         Term (..)
       , termFromEnv
       , smartTermFromEnv
       , Background8
       , Background256
       , Foreground8
       , Foreground256
       , StyleCommon(..)
       , Style8(..)
       , Style256(..)
       , TextSpec (..)
       , defaultColors
       , commonAttrs
       , getTermCodes
       )

--
-- Chunks
--

-- | A chunk is some textual data coupled with a description of what
-- color the text is, attributes like whether it is bold or
-- underlined, etc. The chunk knows what foreground and background
-- colors and what attributes to use for both an 8 color terminal and
-- a 256 color terminal.

data Chunk = Chunk
  { textSpec :: TextSpec
  , text :: Text
  } deriving (Eq, Show, Ord)


instance Str.IsString Chunk where
  fromString s = Chunk mempty (X.pack s)

-- | Creates a 'Chunk' with default colors and no special effects.
fromText :: Text -> Chunk
fromText = Chunk mempty

instance Monoid Chunk where
  mempty = Chunk mempty mempty
  mappend (Chunk s1 t1) (Chunk s2 t2) = Chunk (s1 <> s2) (t1 <> t2)


hPrintChunk :: IO.Handle -> T.Terminal -> Chunk -> IO ()
hPrintChunk h t (Chunk ts x) =
  T.hRunTermOutput h t
  . mconcat
  $ [defaultColors t, codes, txt]
  where
    codes = getTermCodes t ts
    txt = T.termText . X.unpack $ x

-- | Sends a list of chunks to the given handle for printing. Sets up
-- the terminal (this only needs to be done once.) Lazily processes
-- the list of Chunk. See 'putChunks' for notes on how many colors
-- are used.
hPutChunks :: IO.Handle -> Term -> [Chunk] -> IO ()
hPutChunks h t cs = do
  let setup = case t of
        Dumb -> T.setupTerm "dumb"
        TermName s -> T.setupTerm s
  term <- setup
  mapM_ (hPrintChunk h term) cs
  T.hRunTermOutput h term (defaultColors term)
  T.hRunTermOutput h term
    $ case T.getCapability term T.allAttributesOff of
        Nothing -> error $ "System.Console.Rainbow.putChunks: error: "
                   ++ "allAttributesOff failed"
        Just s -> s

-- | Sends a list of chunks to standard output for printing. Sets up
-- the terminal (this only needs to be done once.) Lazily processes
-- the list of Chunk.
--
-- Which colors are used depends upon the 'Term'. If it is 'Dumb',
-- then no colors are used on output. If the 'Term' is specified with
-- 'TermName', the UNIX terminfo library is used to determine how many
-- colors the terminal supports. If it supports at least 256 colors,
-- then 256 colors are used. If it supports at least 8 colors but less
-- than 256 colors, then 256 colors are used. Otherwise, no colors are
-- used. A runtime error will occur if the 'TermName' is not found in
-- the system terminal database.
putChunks :: Term -> [Chunk] -> IO ()
putChunks = hPutChunks IO.stdout

-- | Print one chunk at a time, to a handle
hPutChunk :: IO.Handle -> Chunk -> IO ()
hPutChunk h c = do
  t <- termFromEnv
  hPutChunks h t [c]

-- | Print one chunk at a time, to standard output
putChunk :: Chunk -> IO ()
putChunk = hPutChunk IO.stdout

-- | Print one chunk at a time, to a handle, append a newline
hPutChunkLn :: IO.Handle -> Chunk -> IO ()
hPutChunkLn h c = hPutChunk h c >> IO.hPutStr h "\n"

-- | Print one chunk at a time, to standard output, append a newline
putChunkLn :: Chunk -> IO ()
putChunkLn c = putChunk c >> putStr "\n"

bold8 :: Chunk
bold8 = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scBold = Last (Just True) }}}}
  where
    x = mempty

bold8off :: Chunk
bold8off = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scBold = Last (Just False) }}}}
  where
    x = mempty


underline8 :: Chunk
underline8 = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scUnderline = Last (Just True) }}}}
  where
    x = mempty


underline8off :: Chunk
underline8off = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scUnderline = Last (Just False) }}}}
  where
    x = mempty

flash8 :: Chunk
flash8 = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scFlash = Last (Just True) }}}}
  where
    x = mempty

flash8off :: Chunk
flash8off = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scFlash = Last (Just False) }}}}
  where
    x = mempty


inverse8 :: Chunk
inverse8 = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scInverse = Last (Just True) }}}}
  where
    x = mempty

inverse8off :: Chunk
inverse8off = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scInverse = Last (Just False) }}}}
  where
    x = mempty


underline256 :: Chunk
underline256 = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
      common256 = (common256 (style256 (textSpec x))) {
        scUnderline = Last (Just True) }}}}
  where
    x = mempty


underline256off :: Chunk
underline256off = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
      common256 = (common256 (style256 (textSpec x))) {
        scUnderline = Last (Just False) }}}}
  where
    x = mempty

bold256 :: Chunk
bold256 = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
      common256 = (common256 (style256 (textSpec x))) {
        scBold = Last (Just True) }}}}
  where
    x = mempty

bold256off :: Chunk
bold256off = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
      common256 = (common256 (style256 (textSpec x))) {
        scBold = Last (Just False) }}}}
  where
    x = mempty


inverse256 :: Chunk
inverse256 = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
      common256 = (common256 (style256 (textSpec x))) {
        scInverse = Last (Just True) }}}}
  where
    x = mempty

inverse256off :: Chunk
inverse256off = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
      common256 = (common256 (style256 (textSpec x))) {
        scInverse = Last (Just False) }}}}
  where
    x = mempty


flash256 :: Chunk
flash256 = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
      common256 = (common256 (style256 (textSpec x))) {
        scFlash = Last (Just True) }}}}
  where
    x = mempty


flash256off :: Chunk
flash256off = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
    common256 = (common256 (style256 (textSpec x))) {
    scFlash = Last (Just False) }}}}
  where
    x = mempty


--
-- All
--


-- | Bold. What actually happens when you use Bold is going to depend
-- on your terminal. For example, xterm allows you actually use a bold
-- font for bold, if you have one. Otherwise, it might simulate bold
-- by using overstriking. Another possibility is that your terminal
-- might use a different color to indicate bold. For more details (at
-- least for xterm), look at xterm (1) and search for @boldColors@.
--
-- If your terminal uses a different color for bold, this allows an
-- 8-color terminal to really have 16 colors.
bold :: Chunk
bold = bold8 <> bold256

boldOff :: Chunk
boldOff = bold8off <> bold256off

inverse :: Chunk
inverse = inverse8 <> inverse256

inverseOff :: Chunk
inverseOff = inverse8off <> inverse256off

flash :: Chunk
flash = flash8 <> flash256

flashOff :: Chunk
flashOff = flash8off <> flash256off

underline :: Chunk
underline = underline8 <> underline256

underlineOff :: Chunk
underlineOff = underline8off <> underline256off
