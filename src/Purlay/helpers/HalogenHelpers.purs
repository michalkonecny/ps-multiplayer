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
import Effect.Aff (Aff, Milliseconds)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Web.HTML as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

{-|
  Create an emitter that is controlled by the given Aff action.
-}
affEmitter :: forall a b m . MonadAff m => (HS.Listener a -> Aff b) -> m (HS.Emitter a)
affEmitter affNotifier = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ affNotifier listener
  pure emitter

{-|
  Create an emitter that emits the given value periodically with the given period in milliseconds.
-}
periodicEmitter :: forall m a. MonadAff m => String -> Milliseconds -> a -> m (HS.Emitter a)
periodicEmitter _name periodMs val = 
  affEmitter $ \ listener -> forever do
    Aff.delay periodMs
    -- H.liftEffect $ log $ "emitting on " <> _name
    H.liftEffect $ HS.notify listener val

subscribeToKeyDownUp :: 
  forall m output slots action state. MonadAff m => 
  (H.SubscriptionId -> KeyboardEvent -> action) -> 
  (H.SubscriptionId -> KeyboardEvent -> action) -> 
  H.HalogenM state action slots output m Unit
subscribeToKeyDownUp downHandler upHandler = do
  document <- H.liftEffect $ Window.document =<< Web.window
  H.subscribe' \sid ->
    eventListener
      KET.keydown
      (HTMLDocument.toEventTarget document)
      (map (downHandler sid) <<< KE.fromEvent)
  H.subscribe' \sid ->
    eventListener
      KET.keyup
      (HTMLDocument.toEventTarget document)
      (map (upHandler sid) <<< KE.fromEvent)
      
