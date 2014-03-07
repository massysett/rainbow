module Test.Rainbow.Generators where

import qualified Data.Text as X
import Prelude hiding (last)
import Control.Monad
import Data.Monoid
import Test.QuickCheck
import System.Console.Rainbow.Colors
import System.Console.Rainbow hiding
  (textSpec)
import System.Console.Rainbow.Types hiding
  (style8, style256, textSpec)

-- | Generates one of the valid colors for an 8-color terminal,
-- including the default color.
colors8 :: Gen Color8
colors8 = elements $ c8_default : map snd c8_all

-- | Generates one of the valid colors for an 256-color terminal,
-- including the default color.
colors256 :: Gen Color256
colors256 = elements $ c256_default : map snd c256_all

-- | Generates a foreground color chunk for 8-color terminals.
fgColorChunk8 :: Gen Chunk
fgColorChunk8 = fmap fc8 colors8

-- | Generates a foreground color chunk for 256-color terminals.
fgColorChunk256 :: Gen Chunk
fgColorChunk256 = fmap fc256 colors256

-- | Generates a background color chunk for 8-color terminals.
bgColorChunk8 :: Gen Chunk
bgColorChunk8 = fmap bc8 colors8

-- | Generates a background color chunk for 256-color terminals.
bgColorChunk256 :: Gen Chunk
bgColorChunk256 = fmap bc256 colors256

-- | Generates a color chunk (half foreground, half background) for
-- 8-color terminals.
colorChunk8 :: Gen Chunk
colorChunk8 = oneof [ fgColorChunk8, bgColorChunk8 ]

-- | Generates a color chunk (half foreground, half background) for
-- 256-color terminals.
colorChunk256 :: Gen Chunk
colorChunk256 = oneof [ fgColorChunk256, bgColorChunk256 ]

-- | Generates a color chunk (half for 8-color, half for 256-color).
colorChunk :: Gen Chunk
colorChunk = oneof [ colorChunk8, colorChunk256 ]

last :: a -> Gen (Last a)
last a = frequency [ (3, return $ Last (Just a)),
                     (1, return $ Last Nothing)]

styleCommon :: Gen StyleCommon
styleCommon = liftM4 StyleCommon g g g g
  where
    g = arbitrary >>= last

style8 :: Gen Style8
style8 = liftM3 Style8
  (colors8 >>= last) (colors8 >>= last) styleCommon

style256 :: Gen Style256
style256 = liftM3 Style256
  (colors256 >>= last) (colors256 >>= last) styleCommon

textSpec :: Gen TextSpec
textSpec = liftM2 TextSpec style8 style256

chunk :: X.Text -> Gen Chunk
chunk x = liftM2 Chunk textSpec (return x)

-- | Generates one Chunk for each Text in the list and combines them
-- into one Chunk.
combinedChunks :: [X.Text] -> Gen Chunk
combinedChunks ls = do
  cs <- mapM chunk ls
  return $ foldl (<>) mempty cs
