-- Generates a Cabal file for rainbow-tests using the Cartel package.
--
-- Written for Cartel version 0.10.0.2.

import qualified Cartel as A
import CabalCommon

rainbow :: A.Package
rainbow = A.exactly "rainbow" versionInts

quickcheck :: A.Package
quickcheck = A.closedOpen "QuickCheck" [2,7,5] [2,8]

smallcheck :: A.Package
smallcheck = A.closedOpen "smallcheck" [1,1,1] [1,2]

properties :: A.Properties
properties = sharedProperties
  { A.prName = "rainbow-tests"
  , A.prSynopsis = "Tests and QuickCheck instances to accompany rainbow."

  , A.prExtraSourceFiles =
    [ "README.md"
    , "changelog"
    ]
  }

library
  :: [String]
  -- ^ Library modules
  -> A.Library
library ms = A.Library
  [ A.LibExposedModules ms
  , A.defaultLanguage A.Haskell2010
  , A.ghcOptions ["-Wall"]
  , A.hsSourceDirs ["lib"]
  , A.buildDepends
    [ base
    , terminfo
    , text
    , rainbow
    , quickcheck
    , smallcheck
    ]
  ]

cabal
  :: [String]
  -- ^ Library modules
  -> A.Cabal
cabal ms = A.empty
  { A.cProperties = properties
  , A.cRepositories = [repo]
  , A.cLibrary = Just $ library ms
  }

main :: IO ()
main = do
  ms <- A.modules "lib"
  A.render "genCabal.hs" $ cabal ms
