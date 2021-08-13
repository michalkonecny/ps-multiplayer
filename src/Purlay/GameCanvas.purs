module Purlay.GameCanvas where

import Prelude

import Data.Foldable (traverse_)
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas as Canvas
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Purlay.Coordinator (PeerId)
import Purlay.GameObject (GameObject, unGO)

data Query gstate objinfo go_action a = 
  Q_NewState gstate (List (GameObject gstate objinfo go_action)) a
{-
  Adapted from https://gist.github.com/smilack/11c2fbb48fd85d811999880388e4fa9e
  "PureScript Halogen demo for drawing on a canvas using Hooks"
-}
component ::
  forall input output m gstate objinfo go_action.
  MonadAff m =>
  {my_peerId :: PeerId, initGState :: gstate, width::Number, height::Number} -> 
  H.Component HH.HTML (Query gstate objinfo go_action) input output m
component {my_peerId, initGState, width, height} =
  Hooks.component \{ queryToken } _ -> Hooks.do
    gobjs /\ modifyGObjs <- Hooks.useState List.Nil
    gstate /\ modifyGState <- Hooks.useState initGState
    Hooks.useQuery queryToken case _ of
      Q_NewState gstate' gobjs' _ -> do
        Hooks.modify_ modifyGObjs (const gobjs')
        Hooks.modify_ modifyGState (const gstate')
        pure Nothing
    drawOnCanvas gobjs gstate
    Hooks.pure $ 
      HH.canvas 
        [ HP.id_ "canvas"
        , HP.width  $ Int.ceil width
        , HP.height $ Int.ceil height
        ]
    where
    drawOnCanvas gobjs gstate =
      Hooks.do
        Hooks.captures {} Hooks.useTickEffect do
          mcanvas <- liftEffect $ Canvas.getCanvasElementById "canvas"
          mcontext <- liftEffect $ sequence $ Canvas.getContext2D <$> mcanvas
          traverse_ drawGOs mcontext
          pure Nothing
        Hooks.pure unit
      where
      drawGOs context = liftEffect $ do
        drawBoard
        traverse_ (\go -> (unGO go).draw my_peerId gstate context) gobjs
        where
        drawBoard = do
          Canvas.setFillStyle context "lightgoldenrodyellow"
          Canvas.fillRect context { x: 0.0, y: 0.0, width, height }
