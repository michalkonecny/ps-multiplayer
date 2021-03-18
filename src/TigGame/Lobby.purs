module TigGame.Lobby where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
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

defaultPlayerShape :: Shape
defaultPlayerShape = "😷"
-- playerShape = "X"

type Player = Int
type Shape = String

playersForSelection :: Array Player
playersForSelection = [1,2,3]

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
  shape :: Shape
, players :: Set Player
}

type ConnectingState = {  
  maybeMsg :: Maybe String
}

initialState :: State
initialState = Connecting { maybeMsg: Nothing }

data Action =
    SetWSURL String
  | SetShape Shape
  | SelectPlayer Player Shape

data Query a =
  OtherPlayer Player a

data Output =
    Connected WS.WebSocket
  | SelectedPlayer Player Shape

component :: forall input. H.Component HH.HTML Query input Output Aff
component =
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
      HH.div_ [
        HH.div_ [HH.span_ 
          [HH.text "Enter web-socket URL: ws://", 
          HH.input [HP.value (String.drop 5 defaultWSURL), HE.onValueChange (Just <<< SetWSURL <<< ("ws://" <> _)) ]]]
        , HH.div [HP.class_ (H.ClassName "error")] [HH.text $ fromMaybe "" maybeMsg]]
  render (SelectingPlayer {shape, players}) =
    HH.div_ [ 
      HH.div_ [choosePlayer], 
      HH.div_ [chooseShape]
    ]
    where
    choosePlayer = HH.table_ $ [HH.tr_ $ map playerButton playersForSelection]
    playerButton player = 
      HH.td_ [
        HH.button 
          [ HP.title label, 
            HP.enabled (not $ player `Set.member` players),
            HE.onClick \_ -> Just (SelectPlayer player shape)] 
          [ HH.text label ]]
      where
      label = "Player " <> show player
    chooseShape =
      HH.div_ [
        HH.text "My player's Unicode character:",
        HH.input [HP.value shape, HP.attr (H.AttrName "size") "1", HE.onValueInput (Just <<< SetShape) ]]

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery (OtherPlayer player a) = do
    H.modify_ $ updateSelectingPlayer $ \st -> st { players = Set.insert player st.players }
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
          H.put $ SelectingPlayer { shape: defaultPlayerShape, players: Set.empty }

    SetShape shape -> do
      H.modify_ $ updateSelectingPlayer $ \st -> st { shape = shape }
    SelectPlayer player shape -> do
      H.raise (SelectedPlayer player shape)      
