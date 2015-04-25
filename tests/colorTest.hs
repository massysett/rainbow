module Main where

import Rainbow
import qualified Data.ByteString as BS

colors8 :: [(String, Radiant)]
colors8 =
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

colors256 :: [(String, Radiant)]
colors256
  = ("(no color)", mempty) : map mkColor [minBound..maxBound]
  where
    mkColor w = (show w, color256 w)

colorChunks8ByForeground :: [[Chunk String]]
colorChunks8ByForeground = do
  (fgColorName, fgColor) <- colors8
  (bgColorName, bgColor) <- colors8
  let lbl = "foreground " <> fgColorName <> " background " <> bgColorName
  return [ chunk lbl & fore fgColor & back bgColor
         , chunk "\n"
         ]

colorChunks8ByBackground :: [[Chunk String]]
colorChunks8ByBackground = do
  (bgColorName, bgColor) <- colors8
  (fgColorName, fgColor) <- colors8
  let lbl = "background " <> bgColorName <> " foreground " <> fgColorName
  return [ chunk lbl & fore fgColor & back bgColor
         , chunk "\n"
         ]

colorChunks256ByForeground :: [[Chunk String]]
colorChunks256ByForeground = do
  (fgColorName, fgColor) <- colors256
  (bgColorName, bgColor) <- colors256
  let lbl = "foreground " <> fgColorName <> " background " <> bgColorName
  return [ chunk lbl & fore fgColor & back bgColor
         , chunk "\n"
         ]

colorChunks256ByBackground :: [[Chunk String]]
colorChunks256ByBackground = do
  (bgColorName, bgColor) <- colors256
  (fgColorName, fgColor) <- colors256
  let lbl = "background " <> bgColorName <> " foreground " <> fgColorName
  return [ chunk lbl & fore fgColor & back bgColor
         , chunk "\n"
         ]

sep :: String -> IO ()
sep s = do
  putStrLn ""
  putStrLn ""
  putStrLn $ replicate 40 '='
  putStrLn s

main :: IO ()
main = do
  sep "8 Colors - sorted by foreground color"
  mapM_ BS.putStr . chunksToByteStrings toByteStringsColors8
    . concat $ colorChunks8ByForeground
  sep "8 Colors - sorted by background color"
  mapM_ BS.putStr . chunksToByteStrings toByteStringsColors8
    . concat $ colorChunks8ByBackground
  sep "256 Colors - sorted by foreground color"
  mapM_ BS.putStr . chunksToByteStrings toByteStringsColors256
    . concat $ colorChunks256ByForeground
  sep "256 Colors - sorted by background color"
  mapM_ BS.putStr . chunksToByteStrings toByteStringsColors256
    . concat $ colorChunks256ByBackground
