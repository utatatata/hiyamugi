{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "console"
    , "const"
    , "effect"
    , "either"
    , "foldable-traversable"
    , "generics-rep"
    , "halogen"
    , "maybe"
    , "psci-support"
    , "routing"
    , "routing-duplex"
    , "transformers"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
