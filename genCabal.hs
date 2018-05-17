-- Generates a Cabal file using the Cartel package.
--
-- Written for Cartel version 0.12

import Cartel

rainbowVersion :: [Word]
rainbowVersion = [0,28,0,4]

-- Dependencies

base :: Package
base = closedOpen "base" [4,8] [5]

text :: Package
text = package "text" (gtEq [0,11,2,0])

bytestring :: Package
bytestring = package "bytestring" (gtEq [0,10])

quickCheck :: Package
quickCheck = package "QuickCheck" (gtEq [2,7])

process :: Package
process = package "process" (gtEq [1,2])

lensSimple :: Package
lensSimple = package "lens-simple" (gtEq [0,1,0])

semigroups :: Package
semigroups = closedOpen "semigroups" [0, 18] [0, 19]

commonOptions :: HasBuildInfo a => [a]
commonOptions =
  [ haskell2010
  , ghcOptions ["-Wall"]
  , otherExtensions ["TemplateHaskell"]
  , buildDepends [base, text, bytestring, process, lensSimple]
  , condBlock (invert (impl ghc (gtEq [8, 0]))) (buildDepends [semigroups], []) []
  , hsSourceDirs ["lib"]
  ]

properties :: Properties
properties = blank
  { name = "rainbow"
  , version = rainbowVersion
  , cabalVersion = Just (1,10)
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
  , testedWith = map (\ls -> (ghc, eq ls)) [[7,10,1]]
  }

visualTest
  :: FlagName
  -- ^ Visual flag
  -> [NonEmptyString]
  -- ^ Library modules
  -> String
  -- ^ Name of executable
  -> Section
visualTest fl libMods nm = testSuite nm $
  exitcodeFields (nm ++ ".hs") ++
  [ otherModules libMods
  , hsSourceDirs ["tests"]
  ]
  ++ commonOptions

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
