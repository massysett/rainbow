name: rainbow-tests
version: pv_rainbow
synopsis: Test helpers for rainbow
description:
  Contains generators for rainbow types.

license: BSD3
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
