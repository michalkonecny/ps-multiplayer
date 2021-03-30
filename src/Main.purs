module Main where

import Prelude

import Effect (Effect)
-- import TigGameTable (mainTigGame)
-- import TigGameCanvas (mainTigGame)
import TigGameSmooth (mainTigGame)

main :: Effect Unit
main = mainTigGame
