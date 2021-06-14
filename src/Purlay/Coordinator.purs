module Purlay.Coordinator where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, parseJson, printJsonDecodeError)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Either (Either)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect.Aff (Milliseconds(..), Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Purlay.EitherHelpers (mapLeft, (<||>), (<|||>))
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.EventSource as ES
import Halogen.Query.EventSource as ES.EventSource
import Purlay.Lobby as Lobby
import WSListener (setupWSListener)
import Web.Socket.WebSocket (WebSocket)

{-

A phantom Halogen component with the following:

- state
  - connection to a broadcasting websocket
  - list of participating peers in "power" order (numeric ids)
  - recent "power" measurements for all peers)
  - leader

- actions
  - new peer
  - remove peer
  - change of "power" order
  - change of object state or variable value

- broadcasting to other peers
  - change of state: list of new object states and variable values
    - pulse = empty change of state
  - my "power" measurement
  - new leader (sent when another peer has shown more power for over X seconds)

-}

pulsePeriodMs :: Number
pulsePeriodMs = 100.0

pulseTimeoutMs :: Number
pulseTimeoutMs = 2000.0

type PeerId = Int

data Output 
  = O_Started { my_id :: PeerId, lobby_values :: Lobby.Values }
  | O_StateChanges (Array StateChange)

data Query a
  = Q_StateChanges (Array StateChange)

type StateChange = { name :: String, value :: Json }

type State = {
  m_ws :: Maybe WebSocket
, m_my_id :: Maybe PeerId
, m_last_pulse :: Maybe Instant
, peers :: Array PeerId
, peers_alive :: Map.Map PeerId Instant
, peers_power :: Map.Map PeerId PowerMeasurement
}

type PowerMeasurement = Number -- the larger, the more powerful

i_am_leader :: State -> Boolean
i_am_leader { m_my_id: Just id, peers } = 
  case Array.head peers of
    Just leader -> id == leader
    _ -> false
i_am_leader _ = false

initialState :: State
initialState = {
  m_ws: Nothing
, m_my_id: Nothing
, m_last_pulse: Nothing
, peers: []
, peers_alive: Map.empty
, peers_power: Map.empty
}

data Action
  = HandleLobby Lobby.Output
  | Pulse
  | CheckPeerOrder
  | ReceiveMessageFromPeer String
  -- the following come from peers, via decoding the above String:
  | NewPeerOrder (Array PeerId)
  | NewPowerMeasurement PeerId PowerMeasurement
  | StateChanges (Array StateChange)

messageToAction :: String -> Either String Action
messageToAction msg = do
  json <- (parseJson msg) # (describeErr "Failed to parse message as JSON: ")
  (
    parseStateChanges json
    <|||> 
    parseNewPeerOrder json 
    <||> 
    parseNewPowerMeasurement json 
  ) # describeErrs "Failed to decode JSON:\n"
  where
  parseStateChanges json = do
    ({changes} :: {changes :: Array StateChange}) <- decodeJson json
    pure (StateChanges changes)
  parseNewPeerOrder json = do
    ({peerOrder} :: {peerOrder :: Array PeerId}) <- decodeJson json
    pure (NewPeerOrder peerOrder)
  parseNewPowerMeasurement json = do
    ({peer, power} :: {peer :: PeerId, power :: PowerMeasurement}) <- decodeJson json
    pure (NewPowerMeasurement peer power)

  describeErr :: forall b.String -> Either JsonDecodeError b -> Either String b
  describeErr s = mapLeft (\ err -> s <> (printJsonDecodeError err))
  describeErrs :: forall b.String -> Either (Array JsonDecodeError) b -> Either String b
  describeErrs s = mapLeft (\ errs -> s <> String.joinWith "\n" (map printJsonDecodeError errs))

_lobby :: SProxy "lobby"
_lobby = SProxy

type Slots = 
  ( lobby :: H.Slot Lobby.Query Lobby.Output Int )

component :: forall input. Lobby.ValuesSpec -> H.Component HH.HTML Query input Output Aff
component valuesSpec =
  H.mkComponent
    { 
      initialState: \a -> initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      -- { handleAction = handleAction, initialize = Just Init }
      { handleAction = handleAction }
    }
  where
  render {m_my_id: Nothing} =
    HH.slot _lobby 0 (Lobby.component valuesSpec) unit (Just <<< HandleLobby)
  render {m_my_id: Just my_id} =
    HH.text "<<Coordinator>>"

  handleAction = case _ of
    -- messages from the lobby:
    HandleLobby (Lobby.O_Connected ws) -> do
      H.modify_ $ \st -> st { m_ws = Just ws }
      -- start listening to the websocket:
      void $ H.subscribe $
        ES.EventSource.affEventSource \ emitter -> do
          fiber <- Aff.forkAff $ do
            setupWSListener ws (\msg -> ES.EventSource.emit emitter (ReceiveMessageFromPeer msg))
          pure $ ES.EventSource.Finalizer do
            Aff.killFiber (error "websocket: Event source finalized") fiber
      -- start pulse emitter:
      void $ H.subscribe pulseEmitter

    HandleLobby (Lobby.O_SelectedPlayer peer values) -> do
      -- set my player to the state:
      H.modify_ $ _ { m_my_id = Just peer }
      H.raise $ O_Started { my_id: peer, lobby_values: values }
      -- force a Pulse now to sync with others asap:
      handleAction Pulse

    _ -> pure unit -- TODO
--       -- subscribe to keyboard events:
--       document <- liftEffect $ Web.document =<< Web.window
--       H.subscribe' \sid ->
--         ES.eventListenerEventSource
--           KET.keydown
--           (HTMLDocument.toEventTarget document)
--           (map (HandleKeyDown sid) <<< KE.fromEvent)
--       H.subscribe' \sid ->
--         ES.eventListenerEventSource
--           KET.keyup
--           (HTMLDocument.toEventTarget document)
--           (map (HandleKeyUp sid) <<< KE.fromEvent)
      
--     -- messages from peer players:
--     ReceiveMessageFromPeer msg -> do
--       case messageToAction msg of
--         Left err -> liftEffect $ log err
--         Right action -> handleAction action

--     SetPlayer player playerPiece@(PlayerPiece r) -> do
--       -- get current time:
--       time <- liftEffect now

--       -- update state:
--       H.modify_ $ updateGameState $ \ st -> 
--         st { playersData = Map.insert player {playerPiece, time} st.playersData }
--       passStateToCanvas      

--       -- let the lobby know of this player:
--       void $ H.query _lobby 0 $ H.tell (Lobby.NewPlayer player (Map.singleton "name" r.name))

--     SetIt it -> do
--       H.modify_ $ updateGameState $ _ { it = it, itActive = false }
--       {m_myPlayer} <- H.get
--       pure unit
--       case m_myPlayer of
--         Just myPlayer | it == myPlayer -> do
--           liftAff $ delay (Milliseconds 1000.0) -- 1 second
--           H.modify_ $ updateGameState $ _ { itActive = true }
--         _ -> pure unit
--       -- passStateToCanvas

--     -- control my movement:
--     HandleKeyDown _sid ev -> do
--       case KE.key ev of
--         "ArrowLeft"  -> handleMoveBy (MPt.setAccelX (- speedIncrement))
--         "ArrowRight" -> handleMoveBy (MPt.setAccelX speedIncrement)
--         "ArrowUp"    -> handleMoveBy (MPt.setAccelY (- speedIncrement))
--         "ArrowDown"  -> handleMoveBy (MPt.setAccelY speedIncrement)
--         _ -> pure unit
--     HandleKeyUp _sid ev -> do
--       case KE.key ev of
--         "ArrowLeft"  -> handleMoveBy (MPt.resetAccelX (- speedIncrement))
--         "ArrowRight" -> handleMoveBy (MPt.resetAccelX speedIncrement)
--         "ArrowUp"    -> handleMoveBy (MPt.resetAccelY (- speedIncrement))
--         "ArrowDown"  -> handleMoveBy (MPt.resetAccelY speedIncrement)
--         _ -> pure unit
--     Pulse -> do
--       -- find players who have not sent an update for some time:
--       (Milliseconds timeNow) <- unInstant <$> liftEffect now
--       let timeCutOff = timeNow - pulseTimeoutMs
--       {m_myPlayer, gameState:{playersData,it}} <- H.get
--       let deadPlayers = Map.filter (olderThan timeCutOff) playersData

--       -- tell Lobby to remove these players:
--       if Map.isEmpty deadPlayers then pure unit
--         else do
--           -- liftEffect $ log $ "deadPlayers = " <> show deadPlayers
--           void $ H.query _lobby 0 $ 
--             H.tell (Lobby.ClearPlayers $ Set.toUnfoldable $ Map.keys deadPlayers)

--       -- delete the old players from state:
--       let playersData2 = Map.filter (not <<< olderThan timeCutOff) playersData
--       H.modify_ $ updateGameState $ _ { playersData = playersData2 }

--       -- are we in the game play stage?
--       case m_myPlayer of
--         Nothing -> pure unit
--         Just myPlayer -> do
--           -- let others know we are still alive:
--           handleMoveBy $
--             MPt.move {slowDownRatio}
--             >>> MPt.constrainSpeed {maxSpeed} 
--             >>> MPt.constrainPosWrapAround {minX:0.0, maxX, minY: 0.0, maxY}
          
--           -- check whether "it" exists
--           let itGone = not $ Map.member it playersData2
--           let m_minPlayer = Map.findMin playersData2
--           case m_minPlayer of
--             Just {key: minPlayer} | itGone && minPlayer == myPlayer -> do
--               -- there is no "it" and we are the player with lowerst number, thus we should be it!
--               H.modify_ $ updateGameState $ _ { it = myPlayer }
--               {m_ws} <- H.get
--               liftEffect $ sendIt m_ws myPlayer
--             _ -> pure unit


--   olderThan timeCutOff {time} =
--     let (Milliseconds timeMs) = unInstant time in
--     timeMs < timeCutOff

--   handleMoveBy moveCenter = do
--     {m_ws,m_myPlayer,gameState:{it,itActive,playersData}} <- H.get
--     case m_myPlayer of
--       Nothing -> pure unit
--       Just myPlayer -> do
--         case Map.lookup myPlayer playersData of
--           Nothing -> pure unit
--           Just {playerPiece} -> do
--             -- make the move locally:
--             let newPlayerPiece = updateXYState (\r -> moveCenter r.xyState) playerPiece
--             time <- liftEffect now
--             H.modify_ $ updateGameState $ \st -> 
--               st { playersData = Map.insert myPlayer {playerPiece: newPlayerPiece, time} st.playersData }
--             passStateToCanvas

--             -- send new position to peers:
--             liftEffect $ sendMyPos m_ws {player: myPlayer, playerPiece: newPlayerPiece}

--             -- check for a collision:
--             case getCollision myPlayer newPlayerPiece playersData of
--               Nothing -> pure unit
--               Just (Tuple player newPlayerPiece2) -> do -- collision occurred, bounced off another player
--                   -- change my movement due to the bounce:
--                   H.modify_ $ updateGameState $ \st -> 
--                     st { playersData = Map.insert myPlayer {playerPiece: newPlayerPiece2, time} st.playersData }
--                   passStateToCanvas

--                 -- if I am it, tig them!
--                   when (myPlayer == it && itActive) do
--                     -- gotcha! update it locally:
--                     H.modify_ $ updateGameState $ _ { it = player }
--                     passStateToCanvas
--                     -- and announce new "it":
--                     liftEffect $ sendIt m_ws player

-- adapted from https://milesfrain.github.io/purescript-halogen/guide/04-Lifecycles-Subscriptions.html#implementing-a-timer
pulseEmitter :: forall m. MonadAff m => ES.EventSource m Action
pulseEmitter = ES.EventSource.affEventSource \emitter -> do
  fiber <- Aff.forkAff $ forever do
    Aff.delay $ Milliseconds pulsePeriodMs
    ES.EventSource.emit emitter Pulse

  pure $ ES.EventSource.Finalizer do
    Aff.killFiber (error "pulseEmitter: Event source finalized") fiber
