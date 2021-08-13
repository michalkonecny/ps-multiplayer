{-|
    Module      :  Purlay.Examples.TigGame.PlayerPiece
    Description :  A player's piece
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Purlay.Examples.TigGame.PlayerPiece 
(
  Direction(..), Action(..), ObjInfo, PlayerPiece, new, fromJson
)
where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas as Canvas
import Math (pi)
import Purlay.Coordinator (PeerId)
import Purlay.Examples.TigGame.Global (GState, PlayerId, initialMPt, maxSpeed, maxX, maxY, playerRadius, slowDownRatio, speedIncrement)
import Purlay.GameObject (GameObject(..), HandleAction, unGO)
import Purlay.MovingPoint as MPt
import Purlay.MovingShape (Consistency(..), MovingShape, Shape(..), initMovingAngle, updateXYState)
import Purlay.MovingShape as MShp

type ObjInfo = {
    name :: String
  , playerId :: PlayerId
  }

type PlayerPiece = GameObject GState ObjInfo Action


type State = {
  info :: ObjInfo
, mvshape :: MovingShape
}

new :: ObjInfo -> PlayerPiece
new info@{ playerId } = 
  fromState state
  where
  state = { info, mvshape: gameObjectRecord }
  gameObjectRecord =
    {
      shape: Ball { radius: playerRadius }
    , consistency: Solid
    , scaling: 1.0
    , xyState: initialMPt playerId
    , angleState: initMovingAngle
    }

fromState :: State -> PlayerPiece
fromState state@{ info, mvshape } =
  GameObject {
    info 
  , movingShape: mvshape
  , draw: draw state
  , encode: encodeJson state
  , handleAction: handleAction state
  }

fromJson :: Json -> Either JsonDecodeError PlayerPiece
fromJson json = fromState <$> decodeJson json

draw :: State -> PeerId -> GState -> Canvas.Context2D -> Effect Unit
draw
  {info: {name, playerId}
  , mvshape: {shape: Ball{radius}, xyState: {pos: {x,y}}}}
  peerId
  { it }
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
  Canvas.fillText context name x (y+0.6*radius)
  where
  textSize = Int.round $ 1.6*radius
  playerStyle 
    | is_it = "lightcoral"
    | is_me = "bisque"
    | otherwise = "white"
  is_it = playerId == it
  is_me = playerId == peerId

data Direction = L | R | U | D

data Action 
  = FrameTick
  | PushStart Direction 
  | PushStop  Direction 
  | CheckCollidedWith PlayerPiece

handleAction :: State -> HandleAction GState ObjInfo Action
handleAction {info, mvshape: old_mvshape} gstate (CheckCollidedWith piece) = {
    m_object: map (\mvshape -> fromState {info, mvshape}) m_mvshape
  , m_gstate: Nothing
  }
  where
  m_mvshape = MShp.bounceOff old_mvshape (unGO piece).movingShape
handleAction {info, mvshape: old_mvshape} gstate action = {
    m_object: Just $ fromState {info, mvshape: mvshape action}
  , m_gstate: Nothing
  }
  where
  mvshape FrameTick = 
    flip updateXYState old_mvshape $
      MPt.move {slowDownRatio}
      >>> MPt.constrainSpeed {maxSpeed} 
      >>> MPt.constrainPosWrapAround {minX:0.0, maxX, minY: 0.0, maxY}
  mvshape (PushStart d) = 
    flip updateXYState old_mvshape $
      case d of
        L -> MPt.setAccelX (- speedIncrement)
        R -> MPt.setAccelX (speedIncrement)
        U -> MPt.setAccelY (- speedIncrement)
        D -> MPt.setAccelY (speedIncrement)
  mvshape (PushStop d) = 
    flip updateXYState old_mvshape $
      case d of
        L -> MPt.resetAccelX (- speedIncrement)
        R -> MPt.resetAccelX (speedIncrement)
        U -> MPt.resetAccelY (- speedIncrement)
        D -> MPt.resetAccelY (speedIncrement)
  mvshape _ = old_mvshape
