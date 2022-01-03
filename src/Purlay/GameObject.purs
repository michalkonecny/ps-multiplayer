  module Purlay.GameObject
  ( ApplyAction
  , GameObject(..)
  , GameObjectRec
  , unGO
  , updateRec
  )
  where

import Prelude

import Data.Argonaut (Json)
import Data.Maybe (Maybe)
import Effect (Effect)
import Graphics.Canvas as Canvas
import Purlay.Coordinator (PeerId)
import Purlay.MovingShape (MovingShape)

type ApplyAction gstate objinfo action = 
  gstate -> action -> Maybe (GameObject gstate objinfo action)

data GameObject gstate objinfo action = 
  GameObject (GameObjectRec gstate objinfo action)

type GameObjectRec gstate objinfo action = 
  { 
    info :: objinfo 
  , movingShape :: MovingShape
  , draw :: PeerId -> gstate -> Canvas.Context2D -> Effect Unit
  , encode :: Json
  , applyAction :: ApplyAction gstate objinfo action
  }

unGO :: forall gstate objinfo action. 
  GameObject gstate objinfo action -> GameObjectRec gstate objinfo action
unGO (GameObject gameObjectRec) = gameObjectRec

updateRec :: 
  forall gstate objinfo action. 
  (GameObjectRec gstate objinfo action -> GameObjectRec gstate objinfo action) ->
  GameObject gstate objinfo action -> GameObject gstate objinfo action
updateRec f (GameObject gameObjectRec) = GameObject (f gameObjectRec)
