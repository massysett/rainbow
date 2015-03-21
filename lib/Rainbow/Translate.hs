{-# LANGUAGE OverloadedStrings #-}
module Rainbow.Translate where

import Data.Monoid
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word
import Data.List (intersperse)
import Rainbow.Types
  ( Color8(..), Enum8(..), Color256(..),
    StyleCommon(..), Style8(..), Style256(..), TextSpec)
import qualified Rainbow.Types as T
import Data.Text.Encoding
import System.Process
import Text.Read
import System.Exit
import Control.Monad
import Control.Exception
import qualified System.IO as IO

single :: Char -> [ByteString] -> [ByteString]
single c = ((BS8.singleton c):)

escape :: [ByteString] -> [ByteString]
escape = single '\x1B'

csi :: [ByteString] -> [ByteString]
csi = escape . single '['

sgr :: ([ByteString] -> [ByteString]) -> [ByteString] -> [ByteString]
sgr sq = csi . sq . single 'm'

params :: Show a => [a] -> [ByteString] -> [ByteString]
params cs = ((intersperse ";" . map (BS8.pack . show) $ cs) ++)

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

foreColor8 :: Color8 -> [ByteString] -> [ByteString]
foreColor8 (Color8 maym8) = case maym8 of
  Nothing -> id
  Just m8 -> case m8 of
    E0 -> foreBlack
    E1 -> foreRed
    E2 -> foreGreen
    E3 -> foreYellow
    E4 -> foreBlue
    E5 -> foreMagenta
    E6 -> foreCyan
    E7 -> foreWhite

backColor8 :: Color8 -> [ByteString] -> [ByteString]
backColor8 (Color8 maym8) = case maym8 of
  Nothing -> id
  Just m8 -> case m8 of
    E0 -> backBlack
    E1 -> backRed
    E2 -> backGreen
    E3 -> backYellow
    E4 -> backBlue
    E5 -> backMagenta
    E6 -> backCyan
    E7 -> backWhite

foreColor256 :: Color256 -> [ByteString] -> [ByteString]
foreColor256 (Color256 mayW8) = case mayW8 of
  Nothing -> id
  Just w8 -> fore256 w8

backColor256 :: Color256 -> [ByteString] -> [ByteString]
backColor256 (Color256 mayW8) = case mayW8 of
  Nothing -> id
  Just w8 -> back256 w8

styleCommon :: StyleCommon -> [ByteString] -> [ByteString]
styleCommon (StyleCommon bld fnt ita und bli ivr isb stk)
  = effect bold bld
  . effect faint fnt
  . effect italic ita
  . effect underline und
  . effect blink bli
  . effect inverse ivr
  . effect invisible isb
  . effect strikeout stk
  where
    effect on = maybe id (\x -> if x then on else id)
      . getLast

style8 :: Style8 -> [ByteString] -> [ByteString]
style8 (Style8 f8 b8 sc)
  = effect foreColor8 f8
  . effect backColor8 b8
  . styleCommon sc
  where
    effect on = maybe id on . getLast

style256 :: Style256 -> [ByteString] -> [ByteString]
style256 (Style256 f256 b256 sc)
  = effect foreColor256 f256
  . effect backColor256 b256
  . styleCommon sc
  where
    effect on = maybe id on . getLast

textSpec8 :: TextSpec -> [ByteString] -> [ByteString]
textSpec8 = style8 . T.style8

textSpec256 :: TextSpec -> [ByteString] -> [ByteString]
textSpec256 = style256 . T.style256

-- | Convert a 'T.Chunk' to a list of 'ByteString'; do not show any
-- colors.  When applied to a 'T.Chunk', this function returns a
-- difference list.
toByteStringsColors0 :: T.Chunk -> [ByteString] -> [ByteString]
toByteStringsColors0 c = ((map encodeUtf8 . T.chunkTexts $ c) ++)

-- | Convert a 'T.Chunk' to a list of 'ByteString'; show eight
-- colors.  When applied to a 'T.Chunk', this function returns a
-- difference list.
toByteStringsColors8 :: T.Chunk -> [ByteString] -> [ByteString]
toByteStringsColors8 c
  = normalDefault
  . textSpec8 (T.chunkTextSpec c)
  . ((map encodeUtf8 . T.chunkTexts $ c) ++)
  . normalDefault

-- | Convert a 'T.Chunk' to a list of 'ByteString'; show 256
-- colors.  When applied to a 'T.Chunk', this function returns a
-- difference list.
toByteStringsColors256 :: T.Chunk -> [ByteString] -> [ByteString]
toByteStringsColors256 c
  = normalDefault
  . textSpec256 (T.chunkTextSpec c)
  . ((map encodeUtf8 . T.chunkTexts $ c) ++)
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
  :: IO (T.Chunk -> [ByteString] -> [ByteString])
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
  :: IO.Handle
  -> IO (T.Chunk -> [ByteString] -> [ByteString])
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
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module PrintMyChunks where
-- >
-- > import qualified Data.ByteString as BS
-- > import Rainbow
-- >
-- > myChunks :: [Chunk]
-- > myChunks = [ "Roses" <> fore red, "\n", "Violets" <> fore blue, "\n" ]
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
  :: (T.Chunk -> [ByteString] -> [ByteString])
  -- ^ Function that converts 'T.Chunk' to 'ByteString'.  This
  -- function, when applied to a 'T.Chunk', returns a difference list.
  -> [T.Chunk]
  -> [ByteString]
chunksToByteStrings mk = ($ []) . foldr (.) id . map mk

-- Quick and dirty I/O functions

-- | Writes a 'Chunk' to standard output.  Spawns a child process to
-- read the output of @tput colors@ to determine how many colors to
-- use, for every single chunk.  Therefore, this is not going to win
-- any speed awards.  You are better off using 'chunksToByteStrings'
-- and the functions in "Data.ByteString" to print your 'Chunk's if
-- you are printing a lot of them.
putChunk :: T.Chunk -> IO ()
putChunk ck = do
  mkr <- byteStringMakerFromEnvironment
  mapM_ BS.putStr . chunksToByteStrings mkr $ [ck]

-- | Writes a 'Chunk' to standard output, and appends a newline.
-- Spawns a child process to read the output of @tput colors@ to
-- determine how many colors to use, for every single chunk.
-- Therefore, this is not going to win any speed awards.  You are
-- better off using 'chunksToByteStrings' and the functions in
-- "Data.ByteString" to print your 'Chunk's if you are printing a lot
-- of them.
putChunkLn :: T.Chunk -> IO ()
putChunkLn ck = putChunk ck >> putStrLn ""
