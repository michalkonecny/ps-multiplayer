{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-coroutines"
  , "argonaut"
  , "console"
  , "datetime"
  , "effect"
  , "halogen"
  , "now"
  , "psci-support"
  , "web-socket"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
