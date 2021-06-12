module Purlay.GameObject where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..))
import Purlay.GameObjectRecord (Consistency(..), GameObjectRecord, Shape(..))
import Purlay.JsonHelpers (class DecodeJsonWithSample, class Jsonable, decodeJsonWithSample)
import Purlay.MovingPoint (MovingPoint)

{-|  A game objects has to contain a GameObjectRecord. 

  We work with instances of this class instead of GameObjectRecords directly
  so that the different game object types can have different instances of
  classes such as Drawable.
-}
class (Jsonable go) <= IsGameObject go where
  gameObjectRecord :: go -> GameObjectRecord
  {-| Working with game objects using their GameObjectRecords: Unary function in an applicative functor -}
  updateGameObjectRecordF :: forall f. Applicative f => (GameObjectRecord -> f GameObjectRecord) -> (go -> f go)

{-
  Existential type inspired by https://thimoteus.github.io/posts/2018-09-21-existential-types.html
-}
newtype AnyGameObject = AnyGameObject (forall r. (forall go. IsGameObject go => go -> r) -> r)

anyGameObject :: forall go . IsGameObject go => go -> AnyGameObject
anyGameObject go = AnyGameObject (_ $ go)

instance isGameObjectAnyGameObject :: IsGameObject AnyGameObject where
  gameObjectRecord (AnyGameObject passGO) = passGO gameObjectRecord
  updateGameObjectRecordF fn (AnyGameObject passGO) =
    passGO (map anyGameObject <<< updateGameObjectRecordF fn)

instance jsonableAnyGameObject :: Jsonable AnyGameObject

instance encodeJsonAnyGameObject :: EncodeJson AnyGameObject where
    encodeJson (AnyGameObject passGO) = passGO encodeJson

instance decodeJsonWithSampleAnyGameObject :: DecodeJsonWithSample AnyGameObject where
    decodeJsonWithSample (AnyGameObject passGO) json = 
        passGO (\v -> map anyGameObject (decodeJsonWithSample v json))

{-| Working with game objects using their GameObjectRecords: Unary function -}
updateGameObjectRecord :: 
  forall go. (IsGameObject go) =>
  (GameObjectRecord -> GameObjectRecord) -> (go -> go)
updateGameObjectRecord f go =
  updateGameObjectRecordF (\r unit -> f r) go unit -- using the dummy applicative functor (Unit ->)

updateXYState ::
  forall go. (IsGameObject go) =>
  (GameObjectRecord -> MovingPoint) -> (go -> go)
updateXYState f =
  updateGameObjectRecord (\r -> r {xyState = f r})

{-| Working with game objects using their GameObjectRecords: Binary function with result of another type -}
useGameObjects ::
  forall go1 go2 t. IsGameObject go1 => IsGameObject go2 => 
  (GameObjectRecord -> GameObjectRecord -> t) ->
  go1 -> go2 -> t 
useGameObjects f go1 go2 =
  f (gameObjectRecord go1) (gameObjectRecord go2)

isTouching :: 
  forall go1 go2. IsGameObject go1 => IsGameObject go2 => 
  go1 -> go2 -> Boolean
isTouching = 
  useGameObjects \ 
      {xyState: {pos: {x:x1,y:y1}}, shape: Ball {radius: r1}, consistency: Solid}
      {xyState: {pos: {x:x2,y:y2}}, shape: Ball {radius: r2}, consistency: Solid} ->
    (sqr $ x1-x2) + (sqr $ y1-y2) <= (sqr $ r1+r2)
  where
  sqr a = a * a

{-| Apply a function to a game object's xyState, in coordinates relative to the xyState of another object. -}
relativeTo :: 
  forall go0 go f. Applicative f => IsGameObject go0 => IsGameObject go => 
  go0 -> (go -> f go) -> go -> f go
relativeTo go0 = aux (gameObjectRecord go0)
  where
  aux {xyState: {pos: {x: x0, y: y0}, velo: {x: dx0, y: dy0}}} f  =
    map (updateGameObjectRecord translateBack) <<< f <<< (updateGameObjectRecord translateThere)
    where
    translateThere
        o@{xyState: xyState@{pos: {x,y}, velo: {x: dx,y: dy}}} =
      o{ xyState = xyState {pos = {x: x-x0, y: y-y0}, velo = {x: dx-dx0, y: dy-dy0}} }
    translateBack
        o@{xyState: xyState@{pos: {x,y}, velo: {x: dx,y: dy}}} =
      o{ xyState = xyState {pos = {x: x+x0, y: y+y0}, velo = {x: dx+dx0, y: dy+dy0}} }

bounceOff :: 
  forall go0 go. IsGameObject go0 => IsGameObject go => 
  go0 -> go -> Maybe go
bounceOff otherObj obj = aux (gameObjectRecord otherObj) (gameObjectRecord obj)
  where
  aux {shape: Ball _, consistency: Solid} {shape: Ball _, consistency: Solid}
    | isTouching otherObj obj =
      relativeTo otherObj (updateGameObjectRecordF relCollision) obj
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
