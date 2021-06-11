{-|
    Module      :  Purlay.Lobby
    Description :  A lobby component for a simple multiplayer game
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

   A lobby component for a simple multiplayer game
-}
module Purlay.Lobby where

import Prelude

import Control.SequenceBuildMonad (ae, aes, sb)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Console (log)
import Halogen (ClassName(..), liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML (window)
import Web.HTML.Location (hostname, port)
import Web.HTML.Window (location)
import Web.Socket.ReadyState as ReadyState
import Web.Socket.WebSocket as WS

type Player = Int

type Key = String
type Value = String
type Values = Map.Map Key Value

type ValueSpec = 
  { key :: Key
  , maxLength :: Int
  , description :: String
  , default :: String 
  }
type ValuesSpec = Array ValueSpec

playersForSelection :: Array Player
playersForSelection = [1,2,3,4,5]

data State = Connecting ConnectingState | SelectingPlayer SelectingPlayerState

updateConnecting :: (ConnectingState -> ConnectingState) -> State -> State
updateConnecting fn = case _ of
  Connecting s -> Connecting $ fn s
  state -> state

updateSelectingPlayer :: (SelectingPlayerState -> SelectingPlayerState) -> State -> State
updateSelectingPlayer fn = case _ of
  SelectingPlayer s -> SelectingPlayer $ fn s
  state -> state

type SelectingPlayerState = {  
  values :: Values
, players :: Map Player Values
}

type ConnectingState = {  
  urlInput :: String
, maybeMsg :: Maybe String
}

initialState :: State
initialState = Connecting { urlInput: "ws://localhost:3000", maybeMsg: Nothing }

data Action =
    Init
  | SetWSURL String
  | SetValue Key Value
  | SelectPlayer Player Values

data Query a =
    NewPlayer Player Values a
  | ClearPlayers (Array Player) a

data Output =
    Connected WS.WebSocket
  | SelectedPlayer Player Values

component :: forall input. ValuesSpec -> H.Component HH.HTML Query input Output Aff
component valuesSpec =
  H.mkComponent
    { 
      initialState: \a -> initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      -- { handleAction = handleAction, initialize = Just Init }
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Init }
    }
  where
  render (Connecting {urlInput, maybeMsg}) =
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
  render (SelectingPlayer {values, players}) = 
    HH.div_ $ sb do
      ae$ HH.div_ [chooseValues]
      ae$ HH.div_ [joinGame] 
    where
    joinGame = 
      HH.div_ $ sb do
        ae$ playButton
    playButton = 
      HH.td_ $ sb do
        ae$ HH.button
          (sb do 
            ae$ HP.title label
            ae$ HE.onClick \_ -> Just (SelectPlayer player values))
          [ HH.text label ]
      where
      player = (fromMaybe 0 $ map (_.key) (Map.findMax players)) + 1
      label = "Play"
    chooseValues =
      HH.table [HP.class_ (ClassName "lobby-values")] $ 
        [headerRow] <> map makeRow valuesSpec
      where
      headerRow = 
        HH.tr_ $ sb do
          ae$ HH.th_ []
          ae$ HH.th_ [HH.text "mine"]
          ae$ HH.th [HP.colSpan 4] [HH.text "others"]
      makeRow {key, maxLength, description} =
        HH.tr_ $ sb do
          ae$ HH.td_  [HH.text $ description <> ": "]
          ae$ HH.td_ $ sb do
            ae$ HH.input $ sb do 
              ae$ HP.value (fromMaybe "" $ Map.lookup key values)
              ae$ HP.attr (H.AttrName "size") $ show maxLength
              ae$ HE.onValueInput (Just <<< SetValue key)
          aes$ map playerCell playersArray
        where
        playerCell (Tuple _ pvalues) =
          HH.td_ [HH.text $ fromMaybe "" $ Map.lookup key pvalues]
      playersArray = Map.toUnfoldable players
  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery (NewPlayer player values a) = do
    H.modify_ $ updateSelectingPlayer $ \st -> st { players = Map.insert player values st.players }
    pure Nothing
  handleQuery (ClearPlayers deadPlayers a) = do
    liftEffect $ log $ "ClearPlayers: deadPlayers = " <> (show deadPlayers) 
    let removePlayers mp = foldl (flip Map.delete) mp deadPlayers
    H.modify_ $ updateSelectingPlayer $ \st -> st { players = removePlayers st.players }
    pure Nothing

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
          H.modify_ $ updateConnecting $ \st -> 
            st { urlInput = wsURL, maybeMsg = Just ("Failed to open web-socket at " <> wsURL) }
        else do
          H.raise (Connected ws)
          H.put $ SelectingPlayer { values: defaultValues, players: Map.empty }

    SetValue key value -> do
      H.modify_ $ updateSelectingPlayer $ \st -> st { values = Map.insert key value st.values }
    SelectPlayer player shape -> do
      H.raise (SelectedPlayer player shape)

  getWSURL = do
    loc <- location =<< window
    host <- hostname loc
    p <- port loc
    let prot = if host == "game-ws-broadcast.herokuapp.com" then "wss://" else "ws://"
    pure $ prot <> host <> ":" <> p
  defaultValues = Map.fromFoldable $ map getDefault valuesSpec
    where
    getDefault {key,default} = Tuple key default

