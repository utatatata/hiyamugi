{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "arrays"
    , "canvas"
    , "console"
    , "const"
    , "effect"
    , "either"
    , "foldable-traversable"
    , "generics-rep"
    , "halogen"
    , "integers"
    , "lists"
    , "maybe"
    , "parsing"
    , "psci-support"
    , "routing"
    , "routing-duplex"
    , "transformers"
    , "unicode"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
