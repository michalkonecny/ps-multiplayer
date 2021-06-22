{-|
    Module      :  Purlay.HalogenHelpers
    Description :  Generic Halogen Utilities
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

   Generic Halogen Utilities
-}
module Purlay.HalogenHelpers where

import Prelude

import Control.Monad.Rec.Class (forever)
import Effect.Aff (Milliseconds(..), error)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Query.EventSource as ES
import Halogen.Query.EventSource as ES.EventSource
import Web.HTML as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

-- adapted from https://milesfrain.github.io/purescript-halogen/guide/04-Lifecycles-Subscriptions.html#implementing-a-timer
periodicEmitter :: forall action m. MonadAff m => String -> Number -> action -> ES.EventSource m action
periodicEmitter name periodMs action = ES.EventSource.affEventSource \emitter -> do
  liftEffect $ log $ "starting periodicEmmitter " <> name
  fiber <- Aff.forkAff $ forever do
    Aff.delay $ Milliseconds periodMs
    liftEffect $ log $ "emitting on " <> name
    ES.EventSource.emit emitter action

  pure $ ES.EventSource.Finalizer do
    Aff.killFiber (error $ "emitter " <> name <> ": Event source finalized") fiber

subscribeToKeyDownUp :: 
  forall m output slots action state. MonadAff m => 
  (H.SubscriptionId -> KeyboardEvent -> action) -> 
  (H.SubscriptionId -> KeyboardEvent -> action) -> 
  H.HalogenM state action slots output m Unit
subscribeToKeyDownUp downHandler upHandler = do
  document <- H.liftEffect $ Window.document =<< Web.window
  H.subscribe' \sid ->
    ES.eventListenerEventSource
      KET.keydown
      (HTMLDocument.toEventTarget document)
      (map (downHandler sid) <<< KE.fromEvent)
  H.subscribe' \sid ->
    ES.eventListenerEventSource
      KET.keyup
      (HTMLDocument.toEventTarget document)
      (map (upHandler sid) <<< KE.fromEvent)
      
