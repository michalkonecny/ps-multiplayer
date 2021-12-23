{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ps-multiplayer"
, dependencies =
  [ "aff"
  , "aff-coroutines"
  , "argonaut"
  , "arrays"
  , "canvas"
  , "console"
  , "coroutines"
  , "datetime"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "random"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "web-events"
  , "web-html"
  , "web-socket"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
