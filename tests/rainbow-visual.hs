{-# LANGUAGE OverloadedStrings #-}
module Main where

import Rainbow
import Data.Function ((&))

main :: IO ()
main = do
  putChunksLn
    [ "Red fore, green back " & fore red & back green
    , "Blue fore, magenta back" & fore blue & back magenta ]
  putChunksLn
    [ "Red, bold fore " & fore red & bold
    , "Red, underlined fore " & fore red & underline
    , "Cyan, italic fore" & fore cyan & italic
    ]
  putChunksLn
    [ "Light grey background on 256-color" & back (color256 254) ]
