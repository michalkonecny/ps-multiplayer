module Purlay.Drawable where

import Prelude

import Effect (Effect)
import Graphics.Canvas as Canvas
import Purlay.Coordinator (PeerId)
  
class Drawable gstate t | t -> gstate where
  draw :: { context :: Canvas.Context2D, my_peerId :: PeerId, gstate :: gstate } -> t -> Effect Unit

