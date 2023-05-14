let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall
        sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
  https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221201/packages.dhall
    sha256:d1a68fa15709eaa686515eb5b9950d82c743f7bf73e3d87a4abe9e1be6fda571

in  upstream
  with errors = ../purescript-errors/spago.dhall as Location
    -- mkPackage
    -- [ "prelude", "either", "maybe", "transformers", "control", "effect" ]
    -- "https://github.com/passy/purescript-errors.git"
    -- "v4.1.0"
  with dodo-printer =
      mkPackage
        [ "ansi", "foldable-traversable", "lists", "maybe", "strings" ]
        "https://github.com/natefaubion/purescript-dodo-printer.git"
        "v2.1.0"
  with js-object = ../js-object/spago.dhall as Location
  with js-unsafe-stringify =
      mkPackage
        ([] : List Text)
        "https://github.com/paluh/purescript-js-unsafe-stringify"
        "v0.2.1"
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
        "v1.0.0"
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
