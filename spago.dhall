{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "either"
  , "fixed-points"
  , "foldable-traversable"
  , "lists"
  , "matryoshka"
  , "maybe"
  , "newtype"
  , "node-path"
  , "nullable"
  , "ordered-collections"
  , "pprint"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "psci-support"
  , "simple-json"
  , "strings"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
