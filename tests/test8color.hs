{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow (second)
import Rainbow
import qualified Data.ByteString as BS

effects :: [(Text, Chunk)]
effects =
  [ ("bold", bold8)
  , ("faint", faint8)
  , ("italic", italic8)
  , ("underline", underline8)
  , ("blink", blink8)
  , ("inverse", inverse8)
  , ("invisible", invisible8)
  , ("strikeout", strikeout8)
  ]

colors :: [(Text, Color8)]
colors =
  [ ("(no color)", noColor8)
  , ("black", black8)
  , ("red", red8)
  , ("green", green8)
  , ("yellow", yellow8)
  , ("blue", blue8)
  , ("magenta", magenta8)
  , ("cyan", cyan8)
  , ("white", white8)
  ]

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
  return $ chunkFromText lbl <> fore fgColor
         <> back bgColor
         <> maybe mempty id mayEffect
         <> chunkFromText "\n"

main :: IO ()
main = do
  mapM_ BS.putStr . chunksToByteStrings toByteStringsColors8
    $ colorsAndEffects
