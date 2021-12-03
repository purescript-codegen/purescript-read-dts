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
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "fixed-points"
  , "foldable-traversable"
  , "functions"
  , "js-unsafe-stringify"
  , "lists"
  , "matryoshka"
  , "maybe"
  , "newtype"
  , "node-path"
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
