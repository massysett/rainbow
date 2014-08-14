module CabalCommon where

import qualified Cartel as A

versionInts :: [Int]
versionInts = [0,16,2,0]

-- Package version
version :: A.Version
version = A.Version versionInts

-- Dependencies

base :: A.Package
base = A.closedOpen "base" [4,5,0,0] [4,8,0,0]

terminfo :: A.Package
terminfo = A.closedOpen "terminfo" [0,3,2] [0,5,0,0]

text :: A.Package
text = A.closedOpen "text" [0,11,2,0] [1,2,0,0]

barecheck :: A.Package
barecheck = A.closedOpen "barecheck" [0,2,0,0] [0,3]

sharedProperties :: A.Properties
sharedProperties = A.empty
  { A.prVersion = version
  , A.prLicense = A.BSD3
  , A.prLicenseFile = "LICENSE"
  , A.prCopyright = "Copyright 2013 - 2014 Omari Norman"
  , A.prAuthor = "Omari Norman"
  , A.prMaintainer = "omari@smileystation.com"
  , A.prStability = "Experimental"
  , A.prHomepage = "http://www.github.com/massysett/rainbow"
  , A.prBugReports = "http://www.github.com/massyett/rainbow/issues"
  , A.prCategory = "System"
  }

repo :: A.Repository
repo = A.empty
  { A.repoVcs = A.Git
  , A.repoKind = A.Head
  , A.repoLocation = "git://github.com/massysett/rainbow.git"
  , A.repoBranch = "master"
  }

