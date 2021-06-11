module Purlay.GameObject where

import Prelude

import Data.Maybe (Maybe(..))
import Purlay.GameObjectRecord (Consistency(..), GameObjectRecord, Shape(..))
import Purlay.MovingPoint (MovingPoint)

{-|  A game objects has to contain a GameObjectRecord. 

  We work with instances of this class instead of GameObjectRecords directly
  so that the different game object types can have different instances of
  classes such as Drawable.
-}
class IsGameObject t a | t -> a where
  gameObjectRecord :: t -> GameObjectRecord a
  {-| Working with game objects using their GameObjectRecords: Unary function in an applicative functor -}
  updateGameObjectRecordF :: forall f. Applicative f => (GameObjectRecord a -> f (GameObjectRecord a)) -> (t -> f t)

{-| Working with game objects using their GameObjectRecords: Unary function -}
updateGameObjectRecord :: 
  forall t a. (IsGameObject t a) =>
  (GameObjectRecord a -> GameObjectRecord a) -> (t -> t)
updateGameObjectRecord f go =
  updateGameObjectRecordF (\r unit -> f r) go unit -- using the dummy applicative functor (Unit ->)

updateXYState ::
  forall t a. (IsGameObject t a) =>
  (GameObjectRecord a -> MovingPoint) -> (t -> t)
updateXYState f =
  updateGameObjectRecord (\r -> r {xyState = f r})

{-| Working with game objects using their GameObjectRecords: Binary function with result of another type -}
useGameObjects ::
  forall go1 go2 a1 a2 t. IsGameObject go1 a1 => IsGameObject go2 a2 => 
  (GameObjectRecord a1 -> GameObjectRecord a2 -> t) ->
  go1 -> go2 -> t 
useGameObjects f go1 go2 =
  f (gameObjectRecord go1) (gameObjectRecord go2)

isTouching :: 
  forall go1 go2 a1 a2. IsGameObject go1 a1 => IsGameObject go2 a2 => 
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
  forall go0 a0 go a f. Applicative f => IsGameObject go0 a0 => IsGameObject go a => 
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
  forall go0 a0 go a. IsGameObject go0 a0 => IsGameObject go a => 
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
