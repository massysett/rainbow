{-# LANGUAGE OverloadedStrings #-}
module Main where

import Rainbow
import qualified Data.Text as X
import qualified Data.ByteString as BS

colors8 :: [(Text, Color8)]
colors8 =
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

colors256 :: [(Text, Color256)]
colors256
  = ("(no color)", Color256 Nothing) : map mkColor [minBound..maxBound]
  where
    mkColor w = (X.pack . show $ w, Color256 . Just $ w)

colorChunks8ByForeground :: [Chunk]
colorChunks8ByForeground = do
  (fgColorName, fgColor) <- colors8
  (bgColorName, bgColor) <- colors8
  let lbl = "foreground " <> fgColorName <> " background " <> bgColorName
  return $ fromText lbl <> fore fgColor <> back bgColor <> "\n"

colorChunks8ByBackground :: [Chunk]
colorChunks8ByBackground = do
  (bgColorName, bgColor) <- colors8
  (fgColorName, fgColor) <- colors8
  let lbl = "background " <> bgColorName <> " foreground " <> fgColorName
  return $ fromText lbl <> fore fgColor <> back bgColor <> "\n"

colorChunks256ByForeground :: [Chunk]
colorChunks256ByForeground = do
  (fgColorName, fgColor) <- colors256
  (bgColorName, bgColor) <- colors256
  let lbl = "foreground " <> fgColorName <> " background " <> bgColorName
  return $ fromText lbl <> fore fgColor <> back bgColor <> "\n"

colorChunks256ByBackground :: [Chunk]
colorChunks256ByBackground = do
  (bgColorName, bgColor) <- colors256
  (fgColorName, fgColor) <- colors256
  let lbl = "background " <> bgColorName <> " foreground " <> fgColorName
  return $ fromText lbl <> fore fgColor <> back bgColor <> "\n"

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
    $ colorChunks8ByForeground
  sep "8 Colors - sorted by background color"
  mapM_ BS.putStr . chunksToByteStrings toByteStringsColors8
    $ colorChunks8ByBackground
  sep "256 Colors - sorted by foreground color"
  mapM_ BS.putStr . chunksToByteStrings toByteStringsColors256
    $ colorChunks256ByForeground
  sep "256 Colors - sorted by background color"
  mapM_ BS.putStr . chunksToByteStrings toByteStringsColors256
    $ colorChunks256ByBackground
