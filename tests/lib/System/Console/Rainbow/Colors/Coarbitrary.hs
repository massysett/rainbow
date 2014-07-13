module System.Console.Rainbow.Colors.Coarbitrary where

import Test.QuickCheck
import System.Console.Terminfo.Color.Coarbitrary
import System.Console.Rainbow.Colors

color8 :: Color8 -> Gen b -> Gen b
color8 c = case unColor8 c of
  Nothing -> variant (0 :: Int)
  Just x -> variant (1 :: Int) . color x

color256 :: Color256 -> Gen b -> Gen b
color256 c = case unColor256 c of
  Nothing -> variant (0 :: Int)
  Just x -> variant (1 :: Int) . color x
