{-|
    Module      :  ObjectSpecs
    Description :  Type classes specifying game objects
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

   Type classes specifying game objects
-}
module Purlay.ObjectSpecs where

-- import Prelude

-- import Effect (Effect)
-- import Graphics.Canvas as Canvas
-- import Purlay.MovingPoint (MovingPoint, XY)

-- type DrawParams =
--   {
--     context :: Canvas.Context2D
--   , style :: String
--   }

-- class CanDraw t where
--   draw :: DrawParams -> t -> Effect Unit

-- data Shape 
--   = Ball { radius :: Number }
--   | Rectangle { size :: XY }

-- class HasShape t where
--   getShape :: t -> Shape

-- class CanModifyShape t where
--   modifyShape :: (t -> Shape) -> t -> t

-- class HasPosition t where
--   getCentreXY :: t -> XY

-- class CanModifyPosition t where
--   modifyPosition :: (t -> XY) -> t -> t

-- class MayBeMoving t where
--   getCentreMovingPoint :: t -> MovingPoint

-- class CanModifySpeed t where
--   modifySpeed :: (t -> XY) -> t -> t

-- class CanModifyAccelleration t where
--   modifyAccelleration :: (t -> XY) -> t -> t

-- class CanBounceOff t2 t1 where
--   bounceOff :: t2 -> t1 -> t1
