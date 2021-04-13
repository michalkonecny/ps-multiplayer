{-|
    Module      :  MovingBall
    Description :  A simple 2D smooth moving ball
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

   A simple 2D smooth moving point
-}
module MovingBall where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas as Canvas
import Math (pi)
import MovingPoint (MovingPoint)

type MovingBall = { center :: MovingPoint, name :: String, radius :: Number }

ballsAreTouching :: MovingBall -> MovingBall -> Boolean
ballsAreTouching 
    {center: {pos: {x:x1,y:y1}}, radius: r1}
    {center: {pos: {x:x2,y:y2}}, radius: r2} =
  (sqr $ x1-x2) + (sqr $ y1-y2) <= (sqr $ r1+r2)
  where
  sqr a = a * a

ballBounceOffBall :: MovingBall -> MovingBall -> Maybe MovingBall
ballBounceOffBall otherBall@{radius: r1} ball2@{radius: r2} 
  | ballsAreTouching otherBall ball2 =
    relativeTo otherBall ball2 relCollision
      where
      relCollision ball@{center: center@{pos: {x, y}, velo:{x: dx, y: dy}}} =
        if v <= 0.0 then Nothing -- no collision
        else Just $ ball { center = center { velo = {x: dx + v*x/2.0, y: dy + v*y/2.0} } }
          -- assuming the balls have the same mass
          -- assuming there is no friction or damping
        where
        -- velocity towards other ball stationary at (0,0):
        v = - (y*dy + x*dx) / (x*x + y*y)
  | otherwise = Nothing

relativeTo :: forall f. Applicative f => MovingBall -> MovingBall -> (MovingBall -> f MovingBall) -> f MovingBall
relativeTo
    {center: {pos: {x: x1, y: y1}, velo: {x: dx1, y: dy1}}}
    ball2
    ballFn =
  map translateBack $ ballFn $ translateThere ball2
  where
  translateThere
      ball@{center: center@{pos: {x,y}, velo: {x: dx,y: dy}}} =
    ball { center = center {pos = {x: x-x1, y: y-y1}, velo = {x: dx-dx1, y: dy-dy1}} }
  translateBack
      ball@{center: center@{pos: {x,y}, velo: {x: dx,y: dy}}} =
    ball { center = center {pos = {x: x+x1, y: y+y1}, velo = {x: dx+dx1, y: dy+dy1}} }

drawBall :: Canvas.Context2D -> String -> MovingBall -> Effect Unit
drawBall context style {center: {pos: {x,y}}, name, radius} =  do
    Canvas.setFillStyle context style
    Canvas.fillPath context $ Canvas.arc context 
      { start: 0.0, end: 2.0*pi, radius, x, y }
    Canvas.setFillStyle context "black"
    Canvas.setFont context $ show textSize <> "px sans"
    Canvas.setTextAlign context Canvas.AlignCenter
    -- Canvas.setTextBaseline context Canvas.BaselineTop -- not available in this version yet
    Canvas.fillText context name x (y+0.6*radius)
    where
    textSize = Int.round $ 1.6*radius
