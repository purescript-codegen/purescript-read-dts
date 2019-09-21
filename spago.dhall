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
    , "pprint"
    , "profunctor"
    , "profunctor-lenses"
    , "psci-support"
    , "variant"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
