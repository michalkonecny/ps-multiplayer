{-|
    Module      :  Purlay.Examples.TigGame.PlayerPiece
    Description :  A player's piece
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Purlay.Examples.TigGame.PlayerPiece where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Int as Int
import Graphics.Canvas as Canvas
import Math (pi)
import Purlay.Coordinator (PeerId)
import Purlay.Drawable (class Drawable)
import Purlay.Examples.TigGame.GState (GState(..))
import Purlay.GameObject (class IsGameObject)
import Purlay.GameObjectRecord (Consistency(..), GameObjectRecord, Shape(..), initMovingAngle)
import Purlay.JsonHelpers (class DecodeJsonWithSample, class Jsonable)
import Purlay.MovingPoint (MovingPoint)

newtype PlayerPiece = 
  PlayerPiece { gameObjectRecord :: GameObjectRecord, name :: String, peerId :: PeerId }

derive instance eqPlayerPiece :: Eq PlayerPiece

instance jsonablePlayerPiece :: Jsonable PlayerPiece

instance encodeJsonPlayerPiece :: EncodeJson PlayerPiece where
  encodeJson (PlayerPiece r) = encodeJson r

instance decodeJsonPlayerPiece :: DecodeJson PlayerPiece where
  decodeJson json = 
    case decodeJson json of
      Right r  -> Right $ PlayerPiece r
      Left err -> Left err

instance decodeJsonWithSamplePlayerPiece :: DecodeJsonWithSample PlayerPiece where
    decodeJsonWithSample _ = decodeJson


instance isGameObjectPlayerPiece :: IsGameObject GState PlayerPiece where
  gameObjectRecord (PlayerPiece {gameObjectRecord:r}) = r
  updateGameObjectRecordF f (PlayerPiece pp@{gameObjectRecord:r}) = 
    map (PlayerPiece <<< pp {gameObjectRecord = _}) $ f r

newPlayerPiece :: { peerId :: PeerId, name :: String, xyState :: MovingPoint, radius :: Number } -> PlayerPiece
newPlayerPiece { peerId, name, xyState, radius } = 
  PlayerPiece
  {
    gameObjectRecord: 
      {
        shape: Ball { radius }
      , consistency: Solid
      , scaling: 1.0
      , xyState
      , angleState: initMovingAngle
      }
  , name
  , peerId
  }

instance drawablePlayerPiece :: Drawable GState PlayerPiece where
  draw 
    {context, peerId: my_peerId, gstate: GState { it }} 
    (PlayerPiece {gameObjectRecord: {shape: Ball{radius}, xyState: {pos: {x,y}}}, name, peerId}) 
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
    is_it = peerId == it
    is_me = peerId == my_peerId


