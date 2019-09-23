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
    , "foldable-traversable"
    , "foreign"
    , "psci-support"
    , "strings"
    , "validation"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
