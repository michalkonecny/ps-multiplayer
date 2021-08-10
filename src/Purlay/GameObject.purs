module Purlay.GameObject where

import Prelude

import Data.Argonaut (Json)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Graphics.Canvas as Canvas

type HandleAction global action output = 
  global -> action -> 
    {
      newObject :: GameObject global action output,
      maybe_output :: Maybe output
    }

data GameObject global action output = 
  GameObject { 
    draw :: global -> Canvas.Context2D -> Effect Unit
  , encodeJson :: Json
  , decodeJson :: Json -> Either String (GameObject global action output)
  , handleAction :: HandleAction global action output
  }
