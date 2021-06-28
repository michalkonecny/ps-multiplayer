{-|
    Module      :  Purlay.WSConnector
    Description :  A component for connecting to a broadcast web socket
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    A component for connecting to a broadcast web socket
-}
module Purlay.WSConnector where

import Prelude

import Control.SequenceBuildMonad (ae, sb)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML (window)
import Web.HTML.Location (hostname, port)
import Web.HTML.Window (location)
import Web.Socket.ReadyState as ReadyState
import Web.Socket.WebSocket as WS

data Output =
    O_Connected WS.WebSocket

type State = {  
  urlInput :: String
, maybeMsg :: Maybe String
}

initialState :: State
initialState = { urlInput: "ws://localhost:3000", maybeMsg: Nothing }

data Action =
    Init
  | SetWSURL String

component :: forall query input. H.Component HH.HTML query input Output Aff
component =
  H.mkComponent
    { 
      initialState: \a -> initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      -- { handleAction = handleAction, initialize = Just Init }
      { handleAction = handleAction
      , initialize = Just Init }
    }
  where
  render {urlInput, maybeMsg} =
    HH.div [] [ enterWSURL ]
    where
    enterWSURL = do
      HH.div_ $ sb do
        ae$ HH.div_ $ sb do 
          ae$ HH.span_ $ sb do
            ae$ HH.text "Enter broadcast server web-socket URL: ws://"
            ae$ HH.input $ sb do
              ae$ HP.value (String.drop 5 urlInput)
              ae$ HE.onValueChange (Just <<< SetWSURL <<< ("ws://" <> _))
        ae$ HH.div [HP.class_ (H.ClassName "error")] [HH.text $ fromMaybe "" maybeMsg]

  handleAction = case _ of
    Init -> do
      wsURL <- liftEffect getWSURL
      liftAff $ Aff.delay (Aff.Milliseconds 500.0)
      handleAction $ SetWSURL wsURL
    SetWSURL wsURL -> do
      ws <- liftEffect $ WS.create wsURL []
      liftAff $ Aff.delay (Aff.Milliseconds 100.0) -- allow ws to initialise
      rs <- liftEffect $ WS.readyState ws
      if rs /= ReadyState.Open
        then do
          H.modify_ \st -> 
            st { urlInput = wsURL, maybeMsg = Just ("Failed to open web-socket at " <> wsURL) }
        else do
          H.raise (O_Connected ws)

  getWSURL = do
    loc <- location =<< window
    host <- hostname loc
    p <- port loc
    let prot = if host == "game-ws-broadcast.herokuapp.com" then "wss://" else "ws://"
    pure $ prot <> host <> ":" <> p