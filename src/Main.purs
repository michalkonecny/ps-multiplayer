module Main where

import Prelude

import Effect (Effect)
import TigGameCanvas (mainTigGameCanvas)
-- import TigGameGrid (mainTigGameGrid)

main :: Effect Unit
-- main = mainTigGameGrid
main = mainTigGameCanvas
