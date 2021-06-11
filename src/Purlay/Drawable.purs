module Purlay.Drawable where

import Prelude

import Effect (Effect)
import Graphics.Canvas as Canvas
  
class Drawable t where
  draw :: t -> DrawParams -> Effect Unit

type DrawParams =
  {
    context :: Canvas.Context2D
  , style :: String
  }

