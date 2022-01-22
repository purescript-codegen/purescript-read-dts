{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "fixed-points"
  , "foldable-traversable"
  , "foreign-object"
  , "functions"
  , "identity"
  , "integers"
  , "lists"
  , "matryoshka"
  , "maybe"
  , "monad-loops"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-path"
  , "nullable"
  , "ordered-collections"
  , "partial"
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
