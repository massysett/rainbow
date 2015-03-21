{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow (second)
import Rainbow
import qualified Data.Text as X
import qualified Data.ByteString as BS

effects :: [(Text, Chunk)]
effects =
  [ ("bold", bold256)
  , ("faint", faint256)
  , ("italic", italic256)
  , ("underline", underline256)
  , ("blink", blink256)
  , ("inverse", inverse256)
  , ("invisible", invisible256)
  , ("strikeout", strikeout256)
  ]

colors :: [(Text, Color256)]
colors = ("(no color)", Color256 Nothing) : map mkColor [minBound..maxBound]
  where
    mkColor w = (X.pack . show $ w, Color256 . Just $ w)

maybeEffects :: [(Text, Maybe Chunk)]
maybeEffects = ("(no effect)", Nothing)
  : map (second Just) effects

{-
-- From
-- http://stackoverflow.com/a/22577148/1017252
combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
  where combinations' n k' l@(y:ys)
          | k' == 0   = [[]]
          | k' >= n   = [l]
          | null l    = []
          | otherwise = map (y :) (combinations' (n - 1) (k' - 1) ys)
                            ++ combinations' (n - 1) k' ys 
-}

colorsAndEffects :: [Chunk]
colorsAndEffects = do
  (fgColorName, fgColor) <- colors
  (bgColorName, bgColor) <- colors
  (effectName, mayEffect) <- maybeEffects
  let lbl = "foreground " <> fgColorName <> " background " <> bgColorName
          <> " effect " <> effectName
  return $ chunkFromText lbl <>  fore fgColor
         <>  back bgColor
         <> maybe mempty id mayEffect
         <> "\n"

main :: IO ()
main = do
  mapM_ BS.putStr . chunksToByteStrings toByteStringsColors256
    $ colorsAndEffects
