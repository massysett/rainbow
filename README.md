# rainbow

rainbow helps you build colorful output for both 8- and 256-color
terminals.  It works only on Unix-like operating systems.

rainbow is on Github:

http://www.github.com/massysett/rainbow

and Hackage:

http://hackage.haskell.org/package/rainbow

rainbow is licensed under the BSD license; see the LICENSE file.

## Versioning

rainbow releases are numbered in accordance with the Haskell
Package Versioning Policy.

rainbow does not set its dependencies in accordance with the
Package Versioning Policy, as I do not set upper bounds.  rainbow
is guaranteed to build with the *minimum* versions specified in the
cabal file.  I also include a dependencies.txt file that
documents more recent dependencies that are also known to work.

If you find that rainbow does not build due to dependency problems:
1) please let me know at omari@smileystation.com; 2) feel free to
add appropriate upper bounds or patches to the package as
appropriate; and 3) feel free to add command-line contraints to your
cabal command to get it to build.

## Building

If you get the package from Hackage, it is ready to build with
`cabal install` like any other Haskell package.

If you get it from Github, first you will need to create the cabal
file.  It's built using m4 to avoid redundancies in the file.  To
build the cabal file, simply invoke `make`.

## Generators package

The source tree also includes a package `rainbow-tests` which
includes several generators that other packages might find useful.
