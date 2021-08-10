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
  PlayerId, Global, Direction, Action, Output, Info, PlayerPiece, new
)
where


import Prelude

import Data.Argonaut (Json, encodeJson)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas as Canvas
import Math (pi)
import Purlay.GameObject (GameObject(..), HandleAction)
import Purlay.GameObjectRecord (Consistency(..), Shape(..), GameObjectRecord, initMovingAngle)
import Purlay.MovingPoint (MovingPoint)

type PlayerId = Int

type Global = { it :: PlayerId }

data Direction = DirLeft | DirRight | DirUp | DirDown

data Action = TimeTick | PushStart Direction | PushStop Direction | CollidedWith GameObjectRecord | Tig

data Output

type PlayerPiece = GameObject Global Action Output

type Info = {
    name :: String
  , playerId :: PlayerId
  , is_me :: Boolean 
  }

type State = {
  info :: Info
, rec :: GameObjectRecord
}

new :: 
  Info ->
  { xyState :: MovingPoint
  , radius :: Number } ->
  PlayerPiece
new info { xyState, radius } = 
  fromState state
  where
  state = { info, rec: gameObjectRecord }
  gameObjectRecord =
    {
      shape: Ball { radius }
    , consistency: Solid
    , scaling: 1.0
    , xyState
    , angleState: initMovingAngle
    }

fromState :: State -> PlayerPiece
fromState state =
  GameObject {
    draw: draw state
  , encodeJson: encodeJson state
  , decodeJson: decodeJsonOldState state
  , handleAction: handleAction state
  }

draw :: State -> Global -> Canvas.Context2D -> Effect Unit
draw
  {info: {name, playerId, is_me}
  , rec: {shape: Ball{radius}, xyState: {pos: {x,y}}}}
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

decodeJsonOldState :: State -> Json -> Either String PlayerPiece
decodeJsonOldState { info: oldInfo, rec: oldRec } _ =
  Left "" -- TODO

handleAction :: State -> HandleAction Global Action Output
handleAction state@{info, rec} global action = {
  newObject: fromState state
, maybe_output: Nothing
}
