{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "fixed-points"
  , "foldable-traversable"
  , "functions"
  , "integers"
  , "js-unsafe-stringify"
  , "lists"
  , "matryoshka"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-path"
  , "nonempty"
  , "nullable"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "psci-support"
  , "strings"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "undefined-is-not-a-problem"
  , "unsafe-coerce"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
