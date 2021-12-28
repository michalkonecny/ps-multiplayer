{-|
    Module      :  Purlay.Examples.TigGame.Ball
    Description :  A player's piece
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Purlay.Examples.TigGame.Ball
  ( Action(..)
  , Ball
  , BallId
  , Direction(..)
  , ObjInfo
  , fromJson
  , new
  )
  where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either)
import Data.Int (toNumber)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas as Canvas
import Math (pi)
import Purlay.Coordinator (PeerId)
import Purlay.Examples.TigGame.Global (GState, ballName, ballRadius, initialBallMPt, maxSpeed, maxX, maxY, slowDownRatio, slowDownThreshold, speedIncrement)
import Purlay.GameObject (GameObject(..), HandleAction)
import Purlay.MovingPoint as MPt
import Purlay.MovingShape (MovingShape)
import Purlay.MovingShape as MShp

type ObjInfo = {
  id :: BallId
}

type BallId = Int

type Ball = GameObject GState ObjInfo Action


type State = {
  info :: ObjInfo
, mvshape :: MovingShape
}

new :: BallId -> Ball
new n = 
  fromState { info: { id: n }, mvshape }
  where
  mvshape =
    {
      shape: MShp.Ball { radius: ballRadius }
    , consistency: MShp.Solid
    , scaling: 1.0
    , xyState: initialBallMPt (toNumber n)
    , angleState: MShp.initMovingAngle
    }

fromState :: State -> Ball
fromState state@{ info, mvshape } =
  GameObject {
    info 
  , movingShape: mvshape
  , draw: draw state
  , encode: encodeJson state
  , handleAction: handleAction state
  }

fromJson :: Json -> Either JsonDecodeError Ball
fromJson json = fromState <$> decodeJson json

draw :: State -> PeerId -> GState -> Canvas.Context2D -> Effect Unit
draw
  { mvshape: {shape: MShp.Ball{radius}, xyState: {pos: {x,y}}}}
  _peerId
  _gstate
  context
  =
  do
  Canvas.setFillStyle context playerStyle
  Canvas.fillPath context $ Canvas.arc context 
    { start: 0.0, end: 2.0*pi, radius, x, y }
  Canvas.setFillStyle context "black"
  Canvas.setFont context $ show textSize <> "px sans"
  Canvas.setTextAlign context Canvas.AlignCenter
  -- Canvas.setTextBaseline context Canvas.BaselineTop -- not available in this version yet
  Canvas.fillText context ballName x (y+0.6*radius)
  where
  textSize = Int.round $ 1.6*radius
  playerStyle = "white" 

data Direction = L | R | U | D

data Action 
  = FrameTick
  | PushStart Direction 
  | PushStop  Direction 
  | CheckCollidedWith MovingShape

handleAction :: State -> HandleAction GState ObjInfo Action
handleAction {info, mvshape: old_mvshape} { } action = {
    m_object: map (\mvshape -> fromState {info, mvshape}) $ m_mvshape action
  , m_gstate: Nothing
  }
  where
  m_mvshape FrameTick =
    if mvshape.xyState == old_mvshape.xyState
      then Nothing
      else Just mvshape
    where
    mvshape =
      flip MShp.updateXYState old_mvshape $
        MPt.move {slowDownRatio, slowDownThreshold}
        >>> MPt.constrainSpeed {maxSpeed} 
        >>> MPt.constrainPosWrapAround {minX:0.0, maxX, minY: 0.0, maxY}
  m_mvshape (PushStart d) = Just $
    flip MShp.updateXYState old_mvshape $
      case d of
        L -> MPt.setAccelX (- speedIncrement)
        R -> MPt.setAccelX (speedIncrement)
        U -> MPt.setAccelY (- speedIncrement)
        D -> MPt.setAccelY (speedIncrement)
  m_mvshape (PushStop d) = Just $
    flip MShp.updateXYState old_mvshape $
      case d of
        L -> MPt.resetAccelX (- speedIncrement)
        R -> MPt.resetAccelX (speedIncrement)
        U -> MPt.resetAccelY (- speedIncrement)
        D -> MPt.resetAccelY (speedIncrement)
  m_mvshape (CheckCollidedWith movingShape) =
    MShp.bounceOff movingShape old_mvshape
