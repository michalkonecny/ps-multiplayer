{-|
    Module      :  Purlay.Examples.TigGame.Global
    Description :  Global constants and state
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Purlay.Examples.TigGame.Global where

-- import Prelude

tickPeriod_ms :: Number
tickPeriod_ms = 50.0

maxX :: Number
maxX = 800.0
maxY :: Number
maxY = 800.0

type Name = String

type PlayerId = Int

type GState = { it :: PlayerId, itActive :: Boolean }

initGState :: GState
initGState = {
    it: 0, itActive: true
  }
