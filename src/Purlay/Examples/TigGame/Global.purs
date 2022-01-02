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

import Data.Maybe (Maybe)
import Purlay.GameObject (GameObject)
import Purlay.MovingShape (MovingShape)

-- import Prelude

tickPeriod_ms :: Number
tickPeriod_ms = 50.0

maxX :: Number
maxX = 800.0
maxY :: Number
maxY = 800.0

type Name = String

type PlayerId = Int

type TigState = { it :: PlayerId, itActive :: Boolean }

initTigState :: TigState
initTigState = {
    it: 0, itActive: true
  }

type ObjInfo = {
    name :: Name
  , m_playerId :: Maybe PlayerId
  }

data Direction = L | R | U | D

data ObjAction
  = FrameTick
  | PushStart Direction
  | PushStop  Direction 
  | CheckCollidedWith MovingShape

type TigObject = GameObject TigState ObjInfo ObjAction
