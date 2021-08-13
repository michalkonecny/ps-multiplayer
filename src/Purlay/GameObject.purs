  module Purlay.GameObject 
(
  GameObjectRec, GameObject(..), unGO, HandleAction
)
where

import Prelude

import Data.Argonaut (Json)
import Data.Maybe (Maybe)
import Effect (Effect)
import Graphics.Canvas as Canvas
import Purlay.Coordinator (PeerId)
import Purlay.MovingShape (MovingShape)

type HandleAction gstate objinfo action = 
  gstate -> action -> 
    {
      m_object :: Maybe (GameObject gstate objinfo action)
    , m_gstate :: Maybe gstate
    }

data GameObject gstate objinfo action = 
  GameObject (GameObjectRec gstate objinfo action)

type GameObjectRec gstate objinfo action = 
  { 
    info :: objinfo 
  , movingShape :: MovingShape
  , draw :: PeerId -> gstate -> Canvas.Context2D -> Effect Unit
  , encode :: Json
  , handleAction :: HandleAction gstate objinfo action
  }

unGO :: forall gstate objinfo action. 
  GameObject gstate objinfo action -> GameObjectRec gstate objinfo action
unGO (GameObject gameObjectRec) = gameObjectRec
