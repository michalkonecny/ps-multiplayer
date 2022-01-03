module Purlay.MovingShape where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Purlay.MovingPoint (MovingPoint)
  
type MovingShape = 
  {
    shape :: Shape
  , consistency :: Consistency
  , scaling :: Number
  , xyState :: MovingPoint
  , angleState :: MovingAngle
  }

type MovingAngle = { angle :: Number, velo :: Number, accell :: Number }

initMovingAngle :: MovingAngle
initMovingAngle = { angle: 0.0, velo: 0.0, accell: 0.0 }

data Shape
  = Ball { radius :: Number }
  -- | Rectangle { size :: XY }
  -- | Point -- ie no shape

derive instance eqShape :: Eq Shape

instance encodeJsonShape :: EncodeJson Shape where
  encodeJson (Ball { radius }) = encodeJson ({ shape: "Ball", radius })

instance decodeJsonShape :: DecodeJson Shape where
  decodeJson json = 
    case decodeJson json of 
      Right { shape: "Ball", radius } 
        -> Right $ Ball { radius }
      Right (_ :: { shape :: String, radius :: Number }) 
        -> Left $ UnexpectedValue json
      Left err 
        -> Left err

data Consistency = Solid -- | Container { openings :: Array XYbounds } | Phantom

derive instance eqConsistency :: Eq Consistency

instance encodeJsonConsistency :: EncodeJson Consistency where
  encodeJson Solid = encodeJson "Solid"

instance decodeJsonConsistency :: DecodeJson Consistency where
  decodeJson json = 
    case decodeJson json of 
      Right "Solid" -> Right Solid
      Right _       -> Left $ UnexpectedValue json
      Left err      -> Left err

updateXYState :: (MovingPoint -> MovingPoint) -> (MovingShape -> MovingShape)
updateXYState f mvshape = mvshape { xyState = f mvshape.xyState }

isTouching :: MovingShape -> MovingShape -> Boolean
isTouching 
    {xyState: {pos: {x:x1,y:y1}}, shape: Ball {radius: r1}, consistency: Solid}
    {xyState: {pos: {x:x2,y:y2}}, shape: Ball {radius: r2}, consistency: Solid} =
  (sqr $ x1-x2) + (sqr $ y1-y2) <= (sqr $ r1+r2)
  where
  sqr a = a * a

{-| Apply a function to a game object's xyState, in coordinates relative to the xyState of another object. -}
relativeTo :: 
  forall f. Applicative f => 
  MovingShape -> 
  (MovingShape -> f MovingShape) -> 
  MovingShape -> f MovingShape
relativeTo {xyState: {pos: {x: x0, y: y0}, velo: {x: dx0, y: dy0}}} f =
  map translateBack <<< f <<< translateThere
  where
  translateThere
      o@{xyState: xyState@{pos: {x,y}, velo: {x: dx,y: dy}}} =
    o{ xyState = xyState {pos = {x: x-x0, y: y-y0}, velo = {x: dx-dx0, y: dy-dy0}} }
  translateBack
      o@{xyState: xyState@{pos: {x,y}, velo: {x: dx,y: dy}}} =
    o{ xyState = xyState {pos = {x: x+x0, y: y+y0}, velo = {x: dx+dx0, y: dy+dy0}} }

bounceOff ::
  MovingShape -> MovingShape -> Maybe MovingShape
bounceOff otherR@{shape: Ball _, consistency: Solid} r@{shape: Ball _, consistency: Solid}
  | isTouching otherR r =
    relativeTo otherR relCollision r
      where
      relCollision o@{xyState: xyState@{pos: {x, y}, velo:{x: dx, y: dy}}} =
        if v <= 0.0 then Nothing -- no collision
        else Just $ o{ xyState = xyState{ velo = {x: dx + v*x/2.0, y: dy + v*y/2.0} } }
          -- assuming the balls have the same mass
          -- assuming there is no friction or damping
        where
        -- velocity towards other ball stationary at (0,0):
        v = - (y*dy + x*dx) / (x*x + y*y)
  | otherwise = Nothing

checkCollisionAndBounce ::
  { s1 :: MovingShape, s2 :: MovingShape } -> Maybe { new_s1 :: MovingShape, new_s2 :: MovingShape }
checkCollisionAndBounce { s1: s1@{shape: Ball _, consistency: Solid}, s2: s2@{shape: Ball _, consistency: Solid} }
  | isTouching s1 s2 =
    case relativeTo s2 relCollision s1, relativeTo s1 relCollision s2 of
      Just new_s1, Just new_s2 -> Just { new_s1, new_s2 }
      _, _ -> Nothing
      where
      relCollision o@{xyState: xyState@{pos: {x, y}, velo:{x: dx, y: dy}}} =
        if v <= 0.0 then Nothing -- no collision
        else Just $ o{ xyState = xyState{ velo = {x: dx + v*x/2.0, y: dy + v*y/2.0} } }
          -- assuming the balls have the same mass
          -- assuming there is no friction or damping
        where
        -- velocity towards other ball stationary at (0,0):
        v = - (y*dy + x*dx) / (x*x + y*y)
  | otherwise = Nothing
