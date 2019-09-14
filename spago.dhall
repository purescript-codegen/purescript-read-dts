{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "foreign"
    , "foreign-object"
    , "matryoshka"
    , "node-fs-aff"
    , "profunctor"
    , "profunctor-lenses"
    , "psci-support"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
