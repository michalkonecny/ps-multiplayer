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
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen (ClassName(..), liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type PlayerId = Int

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

type Input = {valuesSpec :: ValuesSpec, my_playerId :: PlayerId }

data Query a =
    Q_NewPlayer PlayerId Values a
  | Q_ClearPlayers (Array PlayerId) a

data Output =
    O_Play Values

type State = {
  my_playerId :: PlayerId
, valuesSpec :: ValuesSpec
, values :: Values
, players :: Map PlayerId Values
}

initialState :: Input -> State
initialState {valuesSpec, my_playerId} = {
    my_playerId
  , valuesSpec
  , values: defaultValues
  , players: Map.empty
  }
  where
  defaultValues = Map.fromFoldable $ map getDefault valuesSpec
    where
    getDefault {key,default} = Tuple key default

data Action =
    SetValue Key Value
  | Play

component :: H.Component HH.HTML Query Input Output Aff
component =
  H.mkComponent
    { 
      initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      -- { handleAction = handleAction, initialize = Just Init }
      { handleAction = handleAction
      , handleQuery = handleQuery }
    }
  where
  render {valuesSpec, values, players} = 
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
            ae$ HE.onClick \_ -> Just Play)
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
  handleQuery (Q_NewPlayer player values a) = do
    H.modify_ \st -> st { players = Map.insert player values st.players }
    pure Nothing
  handleQuery (Q_ClearPlayers deadPlayers a) = do
    liftEffect $ log $ "ClearPlayers: deadPlayers = " <> (show deadPlayers) 
    let removePlayers mp = foldl (flip Map.delete) mp deadPlayers
    H.modify_ \st -> st { players = removePlayers st.players }
    pure Nothing

  handleAction = case _ of
    SetValue key value -> do
      H.modify_ \st -> st { values = Map.insert key value st.values }
    Play -> do
      {values} <- H.get
      H.raise (O_Play values)
