-- Generates a Cabal file using the Cartel package.
--
-- Written for Cartel version 0.10.0.2.

import qualified Cartel as A
import CabalCommon

properties :: A.Properties
properties = sharedProperties
  { A.prName = "rainbow"
  , A.prSynopsis = "Print text to terminal with colors and effects"
  , A.prDescription =
    [ "rainbow helps you print Text chunks to a terminal with colors and effects"
    , "such as bold, underlining, etc. You pair each Text with a description"
    , "of how it should appear. Rainbow works with both 8-color and 256-color"
    , "terminals."
    , ""
    , "rainbow uses the terminfo package which, in turn, needs the full C"
    , "library for ncurses installed, including the development"
    , "headers. Before installing terminfo, you may need to install the"
    , "ncurses headers (for instance, on Debian systems, install the"
    , "libncurses5-dev package.)"
    ]
  , A.prTestedWith =
    map (\ls -> (A.GHC, A.eq ls))
    [ [7,6,3], [7,8,2] ]

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
