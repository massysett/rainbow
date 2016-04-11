{-# LANGUAGE FlexibleInstances #-}

-- | This module contains functions that convert a 'T.Chunk' into
-- 'ByteString's.  Ordinarily everything you need from this module is
-- exported from "Rainbow".
module Rainbow.Translate where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as X
import qualified Data.Text.Encoding as X
import qualified Data.Text.Lazy as XL
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Data.List (intersperse)
import qualified Rainbow.Types as T
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)
import System.Exit (ExitCode(ExitFailure))
import Control.Monad (mzero)
import Control.Exception (try, IOException)
import qualified System.IO as IO


-- | Items that can be rendered.  'render' returns a difference list.
class Renderable a where
  render :: a -> [ByteString] -> [ByteString]

-- | Converts a strict Text to a UTF-8 ByteString.
instance Renderable X.Text where
  render x = (X.encodeUtf8 x :)

-- | Converts a lazy Text to UTF-8 ByteStrings.
instance Renderable XL.Text where
  render = foldr (.) id . map render . XL.toChunks

-- | Strict ByteString is left as-is.
instance Renderable BS.ByteString where
  render x = (x :)

-- | Lazy ByteString is converted to strict chunks.
instance Renderable BSL.ByteString where
  render x = (BSL.toChunks x ++)

-- | Strings are converted first to a strict Text and then to a strict
-- ByteString.
instance Renderable String where
  render x = ((X.encodeUtf8 . X.pack $ x):)

single :: Char -> [ByteString] -> [ByteString]
single c = ((BS8.singleton c):)

escape :: [ByteString] -> [ByteString]
escape = single '\x1B'

csi :: [ByteString] -> [ByteString]
csi = escape . single '['

sgr :: ([ByteString] -> [ByteString]) -> [ByteString] -> [ByteString]
sgr sq = csi . sq . single 'm'

params :: Show a => [a] -> [ByteString] -> [ByteString]
params cs = ((intersperse semi . map (BS8.pack . show) $ cs) ++)
  where
    semi = BS8.singleton ';'

sgrSingle :: Word -> [ByteString] -> [ByteString]
sgrSingle w = sgr $ params [w]

sgrDouble :: Word -> Word -> [ByteString] -> [ByteString]
sgrDouble x y = sgr $ params [x, y]

normalDefault :: [ByteString] -> [ByteString]
normalDefault = sgrSingle 0

bold :: [ByteString] -> [ByteString]
bold = sgrSingle 1

faint :: [ByteString] -> [ByteString]
faint = sgrSingle 2

italic :: [ByteString] -> [ByteString]
italic = sgrSingle 3

underline :: [ByteString] -> [ByteString]
underline = sgrSingle 4

blink :: [ByteString] -> [ByteString]
blink = sgrSingle 5

-- Yes, blink is 5, inverse is 7; 6 is skipped.  In ISO 6429 6 blinks
-- at a different rate.

inverse :: [ByteString] -> [ByteString]
inverse = sgrSingle 7

invisible :: [ByteString] -> [ByteString]
invisible = sgrSingle 8

strikeout :: [ByteString] -> [ByteString]
strikeout = sgrSingle 9

foreBlack :: [ByteString] -> [ByteString]
foreBlack = sgrSingle 30

foreRed :: [ByteString] -> [ByteString]
foreRed = sgrSingle 31

foreGreen :: [ByteString] -> [ByteString]
foreGreen = sgrSingle 32

foreYellow :: [ByteString] -> [ByteString]
foreYellow = sgrSingle 33

foreBlue :: [ByteString] -> [ByteString]
foreBlue = sgrSingle 34

foreMagenta :: [ByteString] -> [ByteString]
foreMagenta = sgrSingle 35

foreCyan :: [ByteString] -> [ByteString]
foreCyan = sgrSingle 36

foreWhite :: [ByteString] -> [ByteString]
foreWhite = sgrSingle 37

-- code 3 8 is skipped

foreDefault :: [ByteString] -> [ByteString]
foreDefault = sgrSingle 39

backBlack :: [ByteString] -> [ByteString]
backBlack = sgrSingle 40

backRed :: [ByteString] -> [ByteString]
backRed = sgrSingle 41

backGreen :: [ByteString] -> [ByteString]
backGreen = sgrSingle 42

backYellow :: [ByteString] -> [ByteString]
backYellow = sgrSingle 43

backBlue :: [ByteString] -> [ByteString]
backBlue = sgrSingle 44

backMagenta :: [ByteString] -> [ByteString]
backMagenta = sgrSingle 45

backCyan :: [ByteString] -> [ByteString]
backCyan = sgrSingle 46

backWhite :: [ByteString] -> [ByteString]
backWhite = sgrSingle 47

-- code 4 8 is skipped

backDefault :: [ByteString] -> [ByteString]
backDefault = sgrSingle 49

fore256 :: Word8 -> [ByteString] -> [ByteString]
fore256 c = sgr $ params [38,5,c]

back256 :: Word8 -> [ByteString] -> [ByteString]
back256 c = sgr $ params [48,5,c]

foreColor8 :: T.Enum8 -> [ByteString] -> [ByteString]
foreColor8 e8 = case e8 of
  T.E0 -> foreBlack
  T.E1 -> foreRed
  T.E2 -> foreGreen
  T.E3 -> foreYellow
  T.E4 -> foreBlue
  T.E5 -> foreMagenta
  T.E6 -> foreCyan
  T.E7 -> foreWhite

backColor8 :: T.Enum8 -> [ByteString] -> [ByteString]
backColor8 e8 = case e8 of
  T.E0 -> backBlack
  T.E1 -> backRed
  T.E2 -> backGreen
  T.E3 -> backYellow
  T.E4 -> backBlue
  T.E5 -> backMagenta
  T.E6 -> backCyan
  T.E7 -> backWhite

renderFormat :: T.Format -> [ByteString] -> [ByteString]
renderFormat (T.Format bld fnt ita und bli ivr isb stk)
  = effect bold bld
  . effect faint fnt
  . effect italic ita
  . effect underline und
  . effect blink bli
  . effect inverse ivr
  . effect invisible isb
  . effect strikeout stk
  where
    effect on x = if x then on else id

renderStyle8 :: T.Style T.Enum8 -> [ByteString] -> [ByteString]
renderStyle8 (T.Style fore back format)
  = effect foreColor8 fore
  . effect backColor8 back
  . renderFormat format
  where
      effect on (T.Color may) = maybe id on may

renderStyle256 :: T.Style Word8 -> [ByteString] -> [ByteString]
renderStyle256 (T.Style fore back format)
  = effect fore256 fore
  . effect back256 back
  . renderFormat format
  where
    effect on (T.Color may) = maybe id on may

toByteStringsColors0
  :: Renderable a
  => T.Chunk a
  -> [ByteString]
  -> [ByteString]
toByteStringsColors0 (T.Chunk _ yn) = render yn

toByteStringsColors8
  :: Renderable a
  => T.Chunk a
  -> [ByteString]
  -> [ByteString]
toByteStringsColors8 (T.Chunk (T.Scheme s8 _) yn)
  = normalDefault
  . renderStyle8 s8
  . render yn
  . normalDefault

toByteStringsColors256
  :: Renderable a
  => T.Chunk a
  -> [ByteString]
  -> [ByteString]
toByteStringsColors256 (T.Chunk (T.Scheme _ s256) yn)
  = normalDefault
  . renderStyle256 s256
  . render yn
  . normalDefault



-- | Spawns a subprocess to read the output of @tput colors@.  If this
-- says there are at least 256 colors are available, returns
-- 'toByteStringsColors256'.  Otherwise, if there are at least 8
-- colors available, returns 'toByteStringsColors8'.  Otherwise,
-- returns 'toByteStringsColors0'.
--
-- If any IO exceptions arise during this process, they are discarded
-- and 'toByteStringsColors0' is returned.
byteStringMakerFromEnvironment
  :: Renderable a
  => IO (T.Chunk a -> [ByteString] -> [ByteString])
byteStringMakerFromEnvironment
  = catcher (fmap f $ readProcessWithExitCode "tput" ["colors"] "")
  where
    f (code, stdOut, _) = maybe toByteStringsColors0 id $ do
      case code of
        ExitFailure _ -> mzero
        _ -> return ()
      numColors <- readMaybe stdOut
      return $ numColorsToFunc numColors
    numColorsToFunc i
      | i >= (256 :: Int) = toByteStringsColors256
      | i >= 8 = toByteStringsColors8
      | otherwise = toByteStringsColors0

    catcher act = fmap g (try act)
      where
        g (Left e) = toByteStringsColors0
          where _types = e :: IOException
        g (Right good) = good

-- | Like 'byteStringMakerFromEnvironment' but also consults a
-- provided 'Handle'.  If the 'Handle' is not a terminal,
-- 'toByteStringsColors0' is returned.  Otherwise, the value of
-- 'byteStringMakerFromEnvironment' is returned.
byteStringMakerFromHandle
  :: Renderable a
  => IO.Handle
  -> IO (T.Chunk a -> [ByteString] -> [ByteString])
byteStringMakerFromHandle h = IO.hIsTerminalDevice h >>= f
  where
    f isTerm | isTerm = byteStringMakerFromEnvironment
             | otherwise = return toByteStringsColors0
          

-- | Convert a list of 'T.Chunk' to a list of 'ByteString'.  The
-- length of the returned list may be longer than the length of the
-- input list.
--
-- So, for example, to print a bunch of chunks to standard output
-- using 256 colors:
--
-- > module PrintMyChunks where
-- >
-- > import qualified Data.ByteString as BS
-- > import Rainbow
-- >
-- > myChunks :: [Chunk String]
-- > myChunks = [ chunk "Roses" & fore red, chunk "\n",
-- >              chunk "Violets" & fore blue, chunk "\n" ]
-- >
-- > myPrintedChunks :: IO ()
-- > myPrintedChunks = mapM_ BS.putStr
-- >                 . chunksToByteStrings toByteStringsColors256
-- >                 $ myChunks
--
-- To use the highest number of colors that this terminal supports:
--
-- > myPrintedChunks' :: IO ()
-- > myPrintedChunks' = do
-- >   printer <- byteStringMakerFromEnvironment
-- >   mapM_ BS.putStr
-- >     . chunksToByteStrings printer
-- >     $ myChunks

chunksToByteStrings
  :: (T.Chunk a -> [ByteString] -> [ByteString])
  -- ^ Function that converts 'T.Chunk' to 'ByteString'.  This
  -- function, when applied to a 'T.Chunk', returns a difference list.
  -> [T.Chunk a]
  -> [ByteString]
chunksToByteStrings mk = ($ []) . foldr (.) id . map mk

-- Quick and dirty I/O functions

-- | Writes a 'T.Chunk' to standard output.  Spawns a child process to
-- read the output of @tput colors@ to determine how many colors to
-- use, for every single chunk.  Therefore, this is not going to win
-- any speed awards.  You are better off using 'chunksToByteStrings'
-- and the functions in "Data.ByteString" to print your 'T.Chunk's if
-- you are printing a lot of them.
putChunk :: Renderable a => T.Chunk a -> IO ()
putChunk ck = do
  mkr <- byteStringMakerFromEnvironment
  mapM_ BS.putStr . chunksToByteStrings mkr $ [ck]

-- | Writes a 'T.Chunk' to standard output, and appends a newline.
-- Spawns a child process to read the output of @tput colors@ to
-- determine how many colors to use, for every single chunk.
-- Therefore, this is not going to win any speed awards.  You are
-- better off using 'chunksToByteStrings' and the functions in
-- "Data.ByteString" to print your 'T.Chunk's if you are printing a lot
-- of them.
putChunkLn :: Renderable a => T.Chunk a -> IO ()
putChunkLn ck = putChunk ck >> putStrLn ""
