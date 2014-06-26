module CabalCommon where

import qualified Cartel as A

-- Package version
version :: A.Version
version = A.Version [0,14,0,2]

-- Dependencies

base :: A.Package
base = A.closedOpen "base" [4,5,0,0] [4,8,0,0]

terminfo :: A.Package
terminfo = A.closedOpen "terminfo" [0,3,2] [0,5,0,0]

text :: A.Package
text = A.closedOpen "text" [0,11,2,0] [1,2,0,0]

