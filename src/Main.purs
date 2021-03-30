module Main where

import Prelude

import Effect (Effect)
import TigGameCanvas (mainTigGameCanvas)
-- import TigGameTable (mainTigGameTable)

main :: Effect Unit
-- main = mainTigGameTable
main = mainTigGameCanvas
