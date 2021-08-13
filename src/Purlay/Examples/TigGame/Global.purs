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

import Prelude

import Control.SequenceBuildMonad (ae, sb)
import Data.Int as Int
import Purlay.Lobby as Lobby
import Purlay.MovingPoint (MovingPoint)
import Purlay.MovingPoint as MPt

-- import Prelude

type PlayerId = Int

tickPeriod_ms :: Number
tickPeriod_ms = 50.0

maxX :: Number
maxX = 800.0
maxY :: Number
maxY = 800.0

maxSpeed :: Number
maxSpeed = 20.0

slowDownRatio :: Number
slowDownRatio = 0.9

speedIncrement :: Number
speedIncrement = 2.0

playerRadius :: Number
playerRadius = 25.0

tigLobbySpec :: Lobby.ValuesSpec
tigLobbySpec = sb do
  ae$
    { key: "name"
    , maxLength: 5
    , description: "Player's name"
    , default: defaultName
    }

type Name = String

defaultName :: Name
defaultName = "😷"

initialMPt :: PlayerId -> MovingPoint
initialMPt player = 
  MPt.constrainPosWrapAround { minX: 0.0, maxX, minY: 0.0, maxY } $
  { pos:
    { x: playerN*107.0, 
      y: playerN*107.0 }
  , velo: { x: 0.0, y: 0.0 }
  , accell: { x: 0.0, y: 0.0 }
  }
  where
  playerN = Int.toNumber player

type GState = { it :: PlayerId, itActive :: Boolean }

initGState :: GState
initGState = {
    it: 0, itActive: true
  }
