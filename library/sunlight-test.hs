module Main where

import Test.Sunlight

inputs = TestInputs
  { tiDescription = Nothing
  , tiCabal = "cabal"
  , tiLowest = ("7.4.1", "ghc-7.4.1", "ghc-pkg-7.4.1")
  , tiDefault = [ ("7.4.1", "ghc-7.4.1", "ghc-pkg-7.4.1")
                , ("7.6.3", "ghc-7.6.3", "ghc-pkg-7.6.3")
                , ("7.8.3", "ghc-7.8.3", "ghc-pkg-7.8.3") ]
  , tiTest = []
  }

main = runTests inputs
