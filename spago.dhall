{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-coroutines"
  , "argonaut"
  , "canvas"
  , "console"
  , "datetime"
  , "effect"
  , "halogen"
  , "halogen-hooks"
  , "now"
  , "psci-support"
  , "random"
  , "web-socket"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
