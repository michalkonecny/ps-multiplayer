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
import Effect (Effect)
import Graphics.Canvas as Canvas
import Math (pi)
import MovingPoint (MovingPoint)

type MovingBall = { center :: MovingPoint, name :: String, radius :: Number }

ballsAreColliding :: MovingBall -> MovingBall -> Boolean
ballsAreColliding 
    {center: {pos: {x:x1,y:y1}}, radius: r1}
    {center: {pos: {x:x2,y:y2}}, radius: r2} =
  (sqr $ x1-x2) + (sqr $ y1-y2) <= (sqr $ r1+r2)
  where
  sqr a = a * a

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
