let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
    https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211030/packages.dhall sha256:5cd7c5696feea3d3f84505d311348b9e90a76c4ce3684930a0ff29606d2d816c
in  upstream
  with
    js-unsafe-stringify = mkPackage
      ([] : List Text)
      "https://github.com/paluh/purescript-js-unsafe-stringify"
      "master"
  with
    matryoshka = mkPackage
      [ "fixed-points", "free", "prelude", "profunctor", "transformers" ]
      "https://github.com/slamdata/purescript-matryoshka.git"
      "v0.4.0"
  with
    pprint = mkPackage
      [ "arrays", "strings", "unfoldable" ]
      "https://github.com/paf31/purescript-pprint.git"
      "v5.0.0"


