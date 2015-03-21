{-# LANGUAGE OverloadedStrings #-}
module Rainbow.Translate where

import Data.Monoid
import qualified Data.ByteString.Char8 as BS8
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
foreBlack = sgrDouble 3 0

foreRed :: [ByteString] -> [ByteString]
foreRed = sgrDouble 3 1

foreGreen :: [ByteString] -> [ByteString]
foreGreen = sgrDouble 3 2

foreYellow :: [ByteString] -> [ByteString]
foreYellow = sgrDouble 3 3

foreBlue :: [ByteString] -> [ByteString]
foreBlue = sgrDouble 3 4

foreMagenta :: [ByteString] -> [ByteString]
foreMagenta = sgrDouble 3 5

foreCyan :: [ByteString] -> [ByteString]
foreCyan = sgrDouble 3 6

foreWhite :: [ByteString] -> [ByteString]
foreWhite = sgrDouble 3 7

-- code 3 8 is skipped

foreDefault :: [ByteString] -> [ByteString]
foreDefault = sgrDouble 3 9

backBlack :: [ByteString] -> [ByteString]
backBlack = sgrDouble 4 0

backRed :: [ByteString] -> [ByteString]
backRed = sgrDouble 4 1

backGreen :: [ByteString] -> [ByteString]
backGreen = sgrDouble 4 2

backYellow :: [ByteString] -> [ByteString]
backYellow = sgrDouble 4 3

backBlue :: [ByteString] -> [ByteString]
backBlue = sgrDouble 4 4

backMagenta :: [ByteString] -> [ByteString]
backMagenta = sgrDouble 4 5

backCyan :: [ByteString] -> [ByteString]
backCyan = sgrDouble 4 6

backWhite :: [ByteString] -> [ByteString]
backWhite = sgrDouble 4 7

-- code 4 8 is skipped

backDefault :: [ByteString] -> [ByteString]
backDefault = sgrDouble 4 9

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
toByteStringsColors0 c = ((map encodeUtf8 . T.text $ c) ++)

-- | Convert a 'T.Chunk' to a list of 'ByteString'; show eight
-- colors.  When applied to a 'T.Chunk', this function returns a
-- difference list.
toByteStringsColors8 :: T.Chunk -> [ByteString] -> [ByteString]
toByteStringsColors8 c
  = normalDefault
  . textSpec8 (T.textSpec c)
  . ((map encodeUtf8 . T.text $ c) ++)

-- | Convert a 'T.Chunk' to a list of 'ByteString'; show 256
-- colors.  When applied to a 'T.Chunk', this function returns a
-- difference list.
toByteStringsColors256 :: T.Chunk -> [ByteString] -> [ByteString]
toByteStringsColors256 c
  = normalDefault
  . textSpec256 (T.textSpec c)
  . ((map encodeUtf8 . T.text $ c) ++)


-- | Use the Terminfo library to return a function that will convert
-- 'T.Chunk' to 'ByteString' in the most colorful way possible.
byteStringMakerFromTerminal
  :: IO (T.Chunk -> [ByteString] -> [ByteString])
byteStringMakerFromTerminal
  = fmap f $ readProcessWithExitCode "tput" ["colors"] ""
  where
    f (code, stdOut, _) = maybe toByteStringsColors0 id $ do
      case code of
        ExitFailure _ -> mzero
        _ -> return ()
      numColors <- readMaybe . filter (`elem` "0123456789") $ stdOut
      return $ numColorsToFunc numColors
    numColorsToFunc i
      | i >= (256 :: Int) = toByteStringsColors256
      | i >= 8 = toByteStringsColors8
      | otherwise = toByteStringsColors0


-- | Convert a list of 'T.Chunk' to a list of 'ByteString'.  The
-- length of the returned list may be longer than the length of the
-- input list.
chunksToByteStrings
  :: (T.Chunk -> [ByteString] -> [ByteString])
  -- ^ Function that converts 'T.Chunk' to 'ByteString'.  This
  -- function, when applied to a 'T.Chunk', returns a difference list.
  -> [T.Chunk]
  -> [ByteString]
chunksToByteStrings mk = ($ []) . foldr (.) id . map mk

