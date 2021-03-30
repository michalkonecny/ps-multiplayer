{-|
    Module      :  MovingPoint
    Description :  A simple 2D smooth moving point
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

   A simple 2D smooth moving point
-}
module MovingPoint where

import Prelude

import Data.Int as Int

type XY = { x :: Number, y :: Number }
type MovingPoint = { pos :: XY, velo :: XY, accell :: XY }

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

move :: Number -> MovingPoint -> MovingPoint
move slowDownRatio mp@{pos: {x,y}, velo: {x:dx, y:dy}, accell: {x:ddx, y:ddy}} = 
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

constrainSpeed :: Number -> MovingPoint -> MovingPoint
constrainSpeed maxSpeed mp@{velo: {x:dx,y:dy}} =
  mp
  {
    velo = 
      {
          x: (dx `min` maxSpeed) `max` (- maxSpeed)
      ,   y: (dy `min` maxSpeed) `max` (- maxSpeed)
      }
  }

constrainLocation :: Number -> Number -> MovingPoint -> MovingPoint
constrainLocation maxX maxY mp@{pos: {x,y}} =
  mp {pos = { x: x `modN` maxX, y: y `modN` maxY }}
  where
  modN a b = a - b * (Int.toNumber (Int.floor (a / b)))