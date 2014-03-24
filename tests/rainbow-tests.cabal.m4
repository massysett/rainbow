name: rainbow-tests
version: pv_rainbow
synopsis: Test helpers for rainbow
description:
  Contains generators for rainbow types.
  .
  Some other packages use these generators.  They are in this
  separate package to keep rainbow from having a dependency
  on QuickCheck.

maintainer: omari@smileystation.com
category: System
license: BSD3
license-file: LICENSE
build-type: Simple
cabal-version: >= 1.8

library
  exposed-modules:
      Test.Rainbow.Generators

  build-depends:
      base >= pv_base && < pv_base_max
    , terminfo >= pv_terminfo
    , text >= pv_text
    , rainbow == pv_rainbow
    , QuickCheck >= pv_quickcheck

  ghc-options: -Wall
