-- Generates a Cabal file using the Cartel package.
--
-- Written for Cartel version 0.12

import Cartel

rainbowVersion :: [Word]
rainbowVersion = [0,22,0,0]

-- Dependencies

base :: Package
base = closedOpen "base" [4,5,0,0] [4,8,0,0]

text :: Package
text = closedOpen "text" [0,11,2,0] [1,3,0,0]

bytestring :: Package
bytestring = closedOpen "bytestring" [0,10] [0,11]

quickCheck :: Package
quickCheck = closedOpen "QuickCheck" [2,7] [2,8]

process :: Package
process = closedOpen "process" [1,2] [1,3]

commonOptions :: HasBuildInfo a => [a]
commonOptions =
  [ haskell2010
  , ghcOptions ["-Wall"]
  , buildDepends [base, text, bytestring, process]
  , hsSourceDirs ["lib"]
  ]

properties :: Properties
properties = blank
  { name = "rainbow"
  , version = rainbowVersion
  , cabalVersion = Just (1,16)
  , buildType = Just simple
  , license = Just bsd3
  , licenseFile = "LICENSE"
  , copyright = "Copyright 2013-2015 Omari Norman"
  , author = "Omari Norman"
  , maintainer = "omari@smileystation.com"
  , stability = "Experimental"
  , homepage = "https://www.github.com/massysett/rainbow"
  , bugReports = "https://www.github.com/massysett/rainbow/issues"
  , category = "System"
  , synopsis = "Print text to terminal with colors and effects"
  , description =
    [ "rainbow helps you print Text chunks to a terminal with colors and effects"
    , "such as bold, underlining, etc. You pair each Text with a description"
    , "of how it should appear. Rainbow works with both 8-color and 256-color"
    , "terminals."
    ]
  , testedWith = map (\ls -> (ghc, eq ls)) [[7,6,3], [7,8,2]]
  }

visualTest
  :: FlagName
  -- ^ Visual flag
  -> [NonEmptyString]
  -- ^ Library modules
  -> String
  -- ^ Name of executable
  -> Section
visualTest fl libMods nm = executable nm $
  [ mainIs (nm ++ ".hs")
  , condBlock (flag fl)
    ( buildable True
    , [ otherModules libMods
      , hsSourceDirs ["tests"]
      ] ++ commonOptions
    )
    [ buildable False ]
  ]

main :: IO ()
main = defaultMain $ do
  libModules <- modules "lib"
  flTests <- makeFlag "visual" (FlagOpts "builds visual tests" False True)
  return
    ( properties
    , exposedModules libModules
      : commonOptions
    , [ githubHead "massysett" "rainbow"

      , testSuite "rainbow-instances" $
        [ exitcodeStdio
        , mainIs "rainbow-instances.hs"
        , otherModules ("Rainbow.QuickCheck" : libModules)
        , hsSourceDirs ["tests"]
        , buildDepends [quickCheck]
        ] ++ commonOptions

      ] ++ map (visualTest flTests libModules)
               [ "test8color", "test256color", "colorTest" ]

    )
