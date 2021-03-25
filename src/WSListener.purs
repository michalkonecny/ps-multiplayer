{-|
    Module      :  WSListener
    Description :  Utility for listening to a web-socket
    Copyright   :  (c) Michal Konecny 2021, authors of purescript-halogen (see link below)
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

   A utility to attach an Aff listener to a web-socket.

   Adapted an extract from https://github.com/purescript-halogen/purescript-halogen/blob/master/examples/driver-websockets/src/Main.purs .  
   The extract has been generalised so that it can be applied to any Aff handler, removing all dependencies on Halogen.

-}
module WSListener
(setupWSListener) 
where

import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foreign (F, Foreign, readString, unsafeToForeign)
import Web.Event.EventTarget as EET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS

-- Setup a listener on the given web socket that sends all messages to the given Halogen IO as a `ReceiveMessage' query.
-- setupWSListener :: forall output. WS.WebSocket -> HA.HalogenIO RootQuery output Aff -> Aff Unit
setupWSListener :: forall a. WS.WebSocket -> (String -> Aff a) -> Aff Unit
setupWSListener ws query =
  CR.runProcess (wsProducer ws CR.$$ wsConsumer query)

-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer :: WS.WebSocket -> CR.Producer String Aff Unit
wsProducer ws =
  CRA.produce \emitter -> do
    listener <- EET.eventListener \ev -> do
      for_ (ME.fromEvent ev) \msgEvent ->
        for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
          CRA.emit emitter msg
    EET.addEventListener
      WSET.onMessage
      listener
      false
      (WS.toEventTarget ws)
  where
    readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
    readHelper read =
      either (const Nothing) Just <<< runExcept <<< read <<< unsafeToForeign

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ReceiveMessage` queries in when it receives inputs from the
-- producer.
wsConsumer :: forall a . (String -> Aff a) -> CR.Consumer String Aff Unit
wsConsumer query = CR.consumer \msg -> do
  void $ query msg
  pure Nothing
