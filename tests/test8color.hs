module Main where

import Control.Arrow (second)
import Rainbow
import qualified Data.ByteString as BS

effects :: [(String, Chunk a -> Chunk a)]
effects =
  [ ("bold", bold)
  , ("faint", faint)
  , ("italic", italic)
  , ("underline", underline)
  , ("blink", blink)
  , ("inverse", inverse)
  , ("invisible", invisible)
  , ("strikeout", strikeout)
  ]

colors :: [(String, Radiant)]
colors =
  [ ("(no color)", mempty)
  , ("black", black)
  , ("red", red)
  , ("green", green)
  , ("yellow", yellow)
  , ("blue", blue)
  , ("magenta", magenta)
  , ("cyan", cyan)
  , ("white", white)
  ]

maybeEffects :: [(String, Maybe (Chunk a -> Chunk a))]
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

colorsAndEffects :: [[Chunk String]]
colorsAndEffects = do
  (fgColorName, fgColor) <- colors
  (bgColorName, bgColor) <- colors
  (effectName, mayEffect) <- maybeEffects
  let lbl = "foreground " ++ fgColorName ++ " background " ++ bgColorName
          ++ " effect " ++ effectName
  return $ [ chunk lbl & fore fgColor
             & back bgColor
             & maybe id id mayEffect
           , chunk "\n"
           ]

main :: IO ()
main
  = mapM_ BS.putStr
  . chunksToByteStrings toByteStringsColors8
  . concat
  $ colorsAndEffects
