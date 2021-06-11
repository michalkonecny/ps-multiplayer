{-|
    Module      :  Purlay.MovingPoint
    Description :  A simple 2D smooth moving point
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

   A simple 2D smooth moving point
-}
module Purlay.MovingPoint where

import Prelude

import Data.Int as Int

type XY = { x :: Number, y :: Number }
type XYbounds = { minX :: Number, maxX :: Number, minY :: Number, maxY :: Number }
type MovingPoint = { pos :: XY, velo :: XY, accell :: XY }

staticMovingPoint :: { pos :: XY } -> MovingPoint
staticMovingPoint { pos } = { pos, velo: {x:0.0, y:0.0}, accell: {x:0.0, y:0.0} }

setAccelX :: Number -> MovingPoint -> MovingPoint
setAccelX ddx ps@{accell: a} = ps { accell = a { x = ddx } }

setAccelY :: Number -> MovingPoint -> MovingPoint
setAccelY ddy ps@{accell: a} = ps { accell = a { y = ddy } }

resetAccelX :: Number -> MovingPoint -> MovingPoint
resetAccelX ddx ps@{accell: a@{x}} 
  | x == ddx = ps { accell = a { x = 0.0 } }
  | otherwise = ps

resetAccelY :: Number -> MovingPoint -> MovingPoint
resetAccelY ddy ps@{accell: a@{y}} 
  | y == ddy = ps { accell = a { y = 0.0 } }
  | otherwise = ps

move :: { slowDownRatio :: Number } -> MovingPoint -> MovingPoint
move { slowDownRatio } mp@{pos: {x,y}, velo: {x:dx, y:dy}, accell: {x:ddx, y:ddy}} = 
  mp
  { 
    pos = 
      { x: x + dx
      , y: y + dy} 
  , velo = -- gradually slow down:
      { x: slowDownRatio * (dx + ddx)
      , y: slowDownRatio * (dy + ddy)
      }
  }

constrainSpeed :: { maxSpeed :: Number } -> MovingPoint -> MovingPoint
constrainSpeed { maxSpeed } mp@{velo: {x:dx,y:dy}} =
  mp
  {
    velo = 
      {
          x: (dx `min` maxSpeed) `max` (- maxSpeed)
      ,   y: (dy `min` maxSpeed) `max` (- maxSpeed)
      }
  }

constrainPosWrapAround :: XYbounds -> MovingPoint -> MovingPoint
constrainPosWrapAround { minX, maxX, minY, maxY} mp@{pos: {x,y}} =
  mp 
    {pos = 
      { x: minX + ((x - minX) `modN` (maxX - minX))
      , y: minY + ((y - minY) `modN` (maxY - minY)) 
      }
    }
  where
  modN a b = a - b * (Int.toNumber (Int.floor (a / b)))