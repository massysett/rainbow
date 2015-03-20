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
import qualified System.Console.Terminfo as Ti

type BsS = [ByteString] -> [ByteString]

single :: Char -> BsS
single c = ((BS8.singleton c):)

escape :: BsS
escape = single '\x1B'

csi :: BsS
csi = escape . single '['

sgr :: BsS -> BsS
sgr sq = csi . sq . single 'm'

params :: Show a => [a] -> BsS
params cs = ((intersperse ";" . map (BS8.pack . show) $ cs) ++)

sgrSingle :: Word -> BsS
sgrSingle w = sgr $ params [w]

sgrDouble :: Word -> Word -> BsS
sgrDouble x y = sgr $ params [x, y]

normalDefault :: BsS
normalDefault = sgrSingle 0

bold :: BsS
bold = sgrSingle 1

underlined :: BsS
underlined = sgrSingle 4

flash :: BsS
flash = sgrSingle 5

-- Yes, flash is 5, inverse is 7; 6 is apparently skipped
inverse :: BsS
inverse = sgrSingle 7

boldOff :: BsS
boldOff = sgrDouble 2 2

underlineOff :: BsS
underlineOff = sgrDouble 2 4

flashOff :: BsS
flashOff = sgrDouble 2 5

inverseOff :: BsS
inverseOff = sgrDouble 2 7

foreBlack :: BsS
foreBlack = sgrDouble 3 0

foreRed :: BsS
foreRed = sgrDouble 3 1

foreGreen :: BsS
foreGreen = sgrDouble 3 2

foreYellow :: BsS
foreYellow = sgrDouble 3 3

foreBlue :: BsS
foreBlue = sgrDouble 3 4

foreMagenta :: BsS
foreMagenta = sgrDouble 3 5

foreCyan :: BsS
foreCyan = sgrDouble 3 6

foreWhite :: BsS
foreWhite = sgrDouble 3 7

-- code 3 8 is skipped

foreDefault :: BsS
foreDefault = sgrDouble 3 9

backBlack :: BsS
backBlack = sgrDouble 4 0

backRed :: BsS
backRed = sgrDouble 4 1

backGreen :: BsS
backGreen = sgrDouble 4 2

backYellow :: BsS
backYellow = sgrDouble 4 3

backBlue :: BsS
backBlue = sgrDouble 4 4

backMagenta :: BsS
backMagenta = sgrDouble 4 5

backCyan :: BsS
backCyan = sgrDouble 4 6

backWhite :: BsS
backWhite = sgrDouble 4 7

-- code 4 8 is skipped

backDefault :: BsS
backDefault = sgrDouble 4 9

fore256 :: Word8 -> BsS
fore256 c = sgr $ params [38,5,c]

back256 :: Word8 -> BsS
back256 c = sgr $ params [48,5,c]

foreColor8 :: Color8 -> BsS
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

backColor8 :: Color8 -> BsS
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

foreColor256 :: Color256 -> BsS
foreColor256 (Color256 mayW8) = case mayW8 of
  Nothing -> id
  Just w8 -> fore256 w8

backColor256 :: Color256 -> BsS
backColor256 (Color256 mayW8) = case mayW8 of
  Nothing -> id
  Just w8 -> back256 w8

styleCommon :: StyleCommon -> BsS
styleCommon (StyleCommon b u f i)
  = effect bold boldOff b
  . effect underlined underlineOff u
  . effect flash flashOff f
  . effect inverse inverseOff i
  where
    effect on off = maybe id (\x -> if x then on else off)
      . getLast

style8 :: Style8 -> BsS
style8 (Style8 f8 b8 sc)
  = effect foreColor8 f8
  . effect backColor8 b8
  . styleCommon sc
  where
    effect on = maybe id on . getLast

style256 :: Style256 -> BsS
style256 (Style256 f256 b256 sc)
  = effect foreColor256 f256
  . effect backColor256 b256
  . styleCommon sc
  where
    effect on = maybe id on . getLast

textSpec8 :: TextSpec -> BsS
textSpec8 = style8 . T.style8

textSpec256 :: TextSpec -> BsS
textSpec256 = style256 . T.style256

toByteStringsColors0 :: T.Chunk -> BsS
toByteStringsColors0 c = ((map encodeUtf8 . T.text $ c) ++)

toByteStringsColors8 :: T.Chunk -> BsS
toByteStringsColors8 c
  = normalDefault
  . textSpec8 (T.textSpec c)
  . ((map encodeUtf8 . T.text $ c) ++)

toByteStringsColors256 :: T.Chunk -> BsS
toByteStringsColors256 c
  = normalDefault
  . textSpec256 (T.textSpec c)
  . ((map encodeUtf8 . T.text $ c) ++)


-- | Use the Terminfo library to return a function that will convert
-- 'T.Chunk' to 'ByteString' in the most colorful way possible.
byteStringMakerFromTerminal
  :: IO (T.Chunk -> [ByteString] -> [ByteString])
byteStringMakerFromTerminal = fmap f Ti.setupTermFromEnv
  where
    f term = case Ti.getCapability term Ti.termColors of
      Nothing -> toByteStringsColors0
      Just cols
        | cols >= 256 -> toByteStringsColors256
        | cols >= 8 -> toByteStringsColors8
        | otherwise -> toByteStringsColors0

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
