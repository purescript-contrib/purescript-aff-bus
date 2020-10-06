{ name = "aff-bus"
, dependencies =
  [ "avar", "console", "effect", "prelude", "psci-support", "refs" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
