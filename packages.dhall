let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211030/packages.dhall sha256:5cd7c5696feea3d3f84505d311348b9e90a76c4ce3684930a0ff29606d2d816c

let overrides = {=}

let additions =
      { matryoshka =
        { dependencies =
          [ "fixed-points", "free", "prelude", "profunctor", "transformers" ]
        , repo = "https://github.com/slamdata/purescript-matryoshka.git"
        , version = "v0.4.0"
        }
      , pprint =
        { dependencies = [ "arrays", "strings", "unfoldable" ]
        , repo = "https://github.com/paf31/purescript-pprint.git"
        , version = "v5.0.0"
        }
      }

in  upstream // overrides // additions
