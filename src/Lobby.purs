{-|
    Module      :  Lobby
    Description :  A lobby component for a simple multiplayer game
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

   A lobby component for a simple multiplayer game
-}
module Lobby where

import Prelude

import Control.SequenceBuildMonad (ae, sb)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Socket.ReadyState as ReadyState
import Web.Socket.WebSocket as WS

defaultWSURL :: String
defaultWSURL = "ws://localhost:3000"

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
  maybeMsg :: Maybe String
}

initialState :: State
initialState = Connecting { maybeMsg: Nothing }

data Action =
    SetWSURL String
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
      , initialize = Just (SetWSURL defaultWSURL) }
    }
  where
  render (Connecting {maybeMsg}) =
    HH.div [] [ enterWSURL ]
    where
    enterWSURL = do
      HH.div_ $ sb do
        ae$ HH.div_ $ sb do 
          ae$ HH.span_ $ sb do
            ae$ HH.text "Enter broadcast server web-socket URL: ws://"
            ae$ HH.input $ sb do
              ae$ HP.value (String.drop 5 defaultWSURL)
              ae$ HE.onValueChange (Just <<< SetWSURL <<< ("ws://" <> _))
        ae$ HH.div [HP.class_ (H.ClassName "error")] [HH.text $ fromMaybe "" maybeMsg]
  render (SelectingPlayer {values, players}) = 
    HH.div_ $ sb do
      ae$ HH.div_ [chooseValues]
      ae$ HH.div_ [choosePlayer] 
    where
    choosePlayer = 
      HH.div_ $ sb do
        ae$ HH.text "Please, select a player number:"
        ae$ HH.table_ $ sb do
          ae$ HH.tr_ $ map playerButton playersForSelection
    playerButton player = 
      HH.td_ $ sb do
        ae$ HH.button
          (sb do 
            ae$ HP.title label
            ae$ HP.enabled (not $ player `Map.member` players)
            ae$ HE.onClick \_ -> Just (SelectPlayer player values))
          [ HH.text label ]
      where
      label = "Player " <> show player
    chooseValues =
      HH.table_ $ map makeRow valuesSpec
      where
      makeRow {key, maxLength, description} =
        HH.tr_ $ sb do
          ae$ HH.text $ description <> ": "
          ae$ HH.input $ sb do 
            ae$ HP.value (fromMaybe "" $ Map.lookup key values)
            ae$ HP.attr (H.AttrName "size") $ show maxLength
            ae$ HE.onValueInput (Just <<< SetValue key)

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery (NewPlayer player values a) = do
    H.modify_ $ updateSelectingPlayer $ \st -> st { players = Map.insert player values st.players }
    pure Nothing
  handleQuery (ClearPlayers deadPlayers a) = do
    let removePlayers mp = foldl (flip Map.delete) mp deadPlayers
    H.modify_ $ updateSelectingPlayer $ \st -> st { players = removePlayers st.players }
    pure Nothing

  handleAction = case _ of
    SetWSURL wsURL -> do
      ws <- liftEffect $ WS.create wsURL []
      liftAff $ Aff.delay (Aff.Milliseconds 100.0) -- allow ws to initialise
      rs <- liftEffect $ WS.readyState ws
      if rs /= ReadyState.Open
        then do
          H.modify_ $ updateConnecting $ \st -> st { maybeMsg = Just ("Failed to open web-socket at " <> wsURL) }
        else do
          H.raise (Connected ws)
          H.put $ SelectingPlayer { values: defaultValues, players: Map.empty }

    SetValue key value -> do
      H.modify_ $ updateSelectingPlayer $ \st -> st { values = Map.insert key value st.values }
    SelectPlayer player shape -> do
      H.raise (SelectedPlayer player shape)

  defaultValues = Map.fromFoldable $ map getDefault valuesSpec
    where
    getDefault {key,default} = Tuple key default

