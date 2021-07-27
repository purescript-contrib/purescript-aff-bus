{ name = "aff-bus"
, dependencies =
  [ "aff"
  , "avar"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "lists"
  , "prelude"
  , "psci-support"
  , "refs"
  , "tailrec"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
