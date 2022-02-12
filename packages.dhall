let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall
        sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211030/packages.dhall
        sha256:5cd7c5696feea3d3f84505d311348b9e90a76c4ce3684930a0ff29606d2d816c

in  upstream
  with dodo-printer =
      mkPackage
        [ "ansi", "foldable-traversable", "lists", "maybe", "strings" ]
        "https://github.com/natefaubion/purescript-dodo-printer.git"
        "v2.1.0"
  with js-unsafe-stringify =
      mkPackage
        ([] : List Text)
        "https://github.com/paluh/purescript-js-unsafe-stringify"
        "master"
  with language-cst-parser =
      mkPackage
        [ "arrays"
        , "const"
        , "effect"
        , "either"
        , "foldable-traversable"
        , "free"
        , "functors"
        , "maybe"
        , "numbers"
        , "ordered-collections"
        , "strings"
        , "transformers"
        , "tuples"
        , "typelevel-prelude"
        ]
        "https://github.com/natefaubion/purescript-language-cst-parser.git"
        "v0.9.1"
  with matryoshka =
      mkPackage
        [ "fixed-points", "free", "prelude", "profunctor", "transformers" ]
        "https://github.com/slamdata/purescript-matryoshka.git"
        "v0.4.0"
  with pprint =
      mkPackage
        [ "arrays", "strings", "unfoldable" ]
        "https://github.com/paf31/purescript-pprint.git"
        "v5.0.0"
  with tidy-codegen =
      mkPackage
        [ "aff"
        , "ansi"
        , "arrays"
        , "avar"
        , "bifunctors"
        , "console"
        , "control"
        , "dodo-printer"
        , "effect"
        , "either"
        , "enums"
        , "exceptions"
        , "filterable"
        , "foldable-traversable"
        , "free"
        , "identity"
        , "integers"
        , "language-cst-parser"
        , "lazy"
        , "lists"
        , "maybe"
        , "newtype"
        , "node-buffer"
        , "node-child-process"
        , "node-fs-aff"
        , "node-path"
        , "node-process"
        , "node-streams"
        , "ordered-collections"
        , "parallel"
        , "partial"
        , "posix-types"
        , "prelude"
        , "record"
        , "safe-coerce"
        , "st"
        , "strings"
        , "tidy"
        , "transformers"
        , "tuples"
        , "type-equality"
        , "unicode"
        ]
        "https://github.com/natefaubion/purescript-tidy-codegen.git"
        "v2.0.0"
  with tidy =
      mkPackage
        [ "arrays"
        , "dodo-printer"
        , "foldable-traversable"
        , "lists"
        , "maybe"
        , "ordered-collections"
        , "partial"
        , "prelude"
        , "language-cst-parser"
        , "strings"
        , "tuples"
        ]
        "https://github.com/natefaubion/purescript-tidy.git"
        "v0.7.0"
