module Purlay.Coordinator where

import Prelude

import Data.Argonaut (class EncodeJson, Json, JsonDecodeError, decodeJson, encodeJson, parseJson, printJsonDecodeError, stringify)
import Data.Array (null, (..))
import Data.Array as Array
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..))
import Data.Foldable (product)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), Aff)
import Effect.Aff as Aff
import Effect.Console (log)
import Effect.Exception (error)
import Effect.Now (now)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.EventSource as ES.EventSource
import Purlay.EitherHelpers (mapLeft, (<||>), (<|||>))
import Purlay.HalogenHelpers (periodicEmitter)
import Purlay.Lobby as Lobby
import WSListener (setupWSListener)
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WS

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

pulsePeriod_ms :: Number
pulsePeriod_ms = 500.0

pulseTimeoutMs :: Number
pulseTimeoutMs = 2000.0

checkPowerPeriod_ms :: Number
checkPowerPeriod_ms = 5000.0

type PeerId = Int

data Output 
  = O_Started { my_id :: PeerId, lobby_values :: Lobby.Values }
  | O_StateChanges (Array StateChange)
  | O_RemovePeers (Array PeerId)

data Query a
  = Q_StateChanges (Array StateChange) a

type StateChange = { name :: String, value :: Json }

type State = {
  m_ws :: Maybe WebSocket
, m_my_info :: Maybe { peerId :: PeerId, values :: Lobby.Values }
, peers_order :: Array PeerId
, peers_alive :: Map.Map PeerId Instant
, peers_power :: Map.Map PeerId PowerMeasurement
, peers_values :: Map.Map PeerId Lobby.Values
}

type PowerMeasurement = Number -- the larger, the more powerful

i_am_leader :: State -> Boolean
i_am_leader { m_my_info: Just {peerId}, peers_order } = 
  case Array.head peers_order of
    Just leader -> peerId == leader
    _ -> false
i_am_leader _ = false

initialState :: State
initialState = {
  m_ws: Nothing
, m_my_info: Nothing
, peers_order: []
, peers_alive: Map.empty
, peers_power: Map.empty
, peers_values: Map.empty
}

data Action
  = HandleLobby Lobby.Output
  | OutgoingPulse
  | MeasurePower
  | CheckPeerOrder
  | OutgoingStateChanges (Array StateChange)
  | ReceiveMessageFromPeer String
  -- the following come from peers, via decoding the above String:
  | IncomingPulse PeerId
  | IncomingPeersOrder (Array PeerId)
  | IncomingPowerMeasurementAndValues {peerId::PeerId, power::PowerMeasurement, values::Lobby.Values}
  | IncomingStateChanges (Array StateChange)

messageToAction :: String -> Either String Action
messageToAction msg = do
  json <- (parseJson msg) # (describeErr "Failed to parse message as JSON: ")
  (
    parsePeerPulse json 
    <|||> 
    parseStateChanges json
    <||> 
    parseNewPeerOrder json 
    <||> 
    parseNewPowerMeasurement json 
  ) # describeErrs "Failed to decode JSON:\n"
  where
  parsePeerPulse json = do
    ({pulse: peerId} :: {pulse :: PeerId}) <- decodeJson json
    pure (IncomingPulse peerId)
  parseStateChanges json = do
    ({changes} :: {changes :: Array StateChange}) <- decodeJson json
    pure (IncomingStateChanges changes)
  parseNewPeerOrder json = do
    ({peers_order} :: {peers_order :: Array PeerId}) <- decodeJson json
    pure (IncomingPeersOrder peers_order)
  parseNewPowerMeasurement json = do
    ({peerId, power, values} :: 
      {peerId :: PeerId, power :: PowerMeasurement, values :: Lobby.Values}) <- decodeJson json
    pure (IncomingPowerMeasurementAndValues {peerId, power,values})

  describeErr :: forall b.String -> Either JsonDecodeError b -> Either String b
  describeErr s = mapLeft (\ err -> s <> (printJsonDecodeError err))
  describeErrs :: forall b.String -> Either (Array JsonDecodeError) b -> Either String b
  describeErrs s = mapLeft (\ errs -> s <> String.joinWith "\n" (map printJsonDecodeError errs))

_lobby :: SProxy "lobby"
_lobby = SProxy

-- type Slots = 
--   ( lobby :: H.Slot Lobby.Query Lobby.Output Int )

component :: forall input. Lobby.ValuesSpec -> H.Component HH.HTML Query input Output Aff
component valuesSpec =
  H.mkComponent
    { 
      initialState: \a -> initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      -- { handleAction = handleAction, initialize = Just Init }
      { handleAction = handleAction, handleQuery = handleQuery }
    }
  where
  render {m_my_info: Nothing, peers_order, peers_alive, peers_power} =
    HH.div_ 
      [
        HH.slot _lobby 111 (Lobby.component valuesSpec) unit (Just <<< HandleLobby)
      , HH.text $ show peers_order <> "; " <> show peers_alive <> "; " <> show peers_power
      ]
  render {m_my_info: Just my_info, peers_order, peers_alive, peers_power} =
    HH.text $ show my_info <> "; " <> show peers_order <> "; " <> show peers_alive <> "; " <> show peers_power

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    Q_StateChanges changes _ -> do
      handleAction $ OutgoingStateChanges changes
      pure Nothing

  handleAction = case _ of
    -- messages from peers:
    ReceiveMessageFromPeer msg -> do
      case messageToAction msg of
        Left err -> liftEffect $ log err
        Right action -> handleAction action

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

    HandleLobby (Lobby.O_SelectedPlayer peerId values) -> do
      -- set my player to the state:
      H.modify_ $ _ { m_my_info = Just {peerId, values} }
      H.raise $ O_Started { my_id: peerId, lobby_values: values }

      -- start periodic emitters for Pulse and MeasurePower actions:
      void $ H.subscribe $ periodicEmitter "Pulse" pulsePeriod_ms OutgoingPulse
      void $ H.subscribe $ periodicEmitter "MeasurePower" checkPowerPeriod_ms MeasurePower

      -- force a CheckPeerOrder now to sync with others asap:
      handleAction MeasurePower

    MeasurePower -> do
      -- liftEffect $ log "MeasurePower"
      state@{m_ws, m_my_info, peers_power} <- H.get
      case m_my_info of
        Nothing -> pure unit
        Just {peerId,values} -> do
          power <- liftEffect measurePower
          liftEffect $ broadcastToPeers m_ws {peerId, power, values}
          let peers_power2 = Map.insert peerId power peers_power
          H.modify_ $ _ { peers_power = peers_power2 }
      handleAction CheckPeerOrder

    CheckPeerOrder -> do
      -- liftEffect $ log "CheckPeerOrder"
      state@{m_ws, m_my_info, peers_power, peers_order: peers_order1} <- H.get
      when (i_am_leader state || null peers_order1) do
        let compSnd (Tuple _ p1) (Tuple _ p2) = compare (p1::Number) p2
        let peers_order2 = map fst $ Array.sortBy compSnd $ Map.toUnfoldableUnordered peers_power
        liftEffect $ log $ "CheckPeerOrder: peers_order2 = " <> show peers_order2
        H.modify_ $ _ { peers_order = peers_order2 }
        when (i_am_leader state) do
          liftEffect $ broadcastToPeers m_ws {peers_order: peers_order2}

    IncomingPowerMeasurementAndValues {peerId, power, values} -> do
      H.modify_ \s -> s
        { peers_power  = Map.insert peerId power s.peers_power 
        , peers_values = Map.insert peerId values s.peers_values
        }
      void $ H.query _lobby 111 $ H.tell (Lobby.Q_NewPlayer peerId values)

    IncomingPeersOrder peers_order -> do
      H.modify_ $ _ {peers_order = peers_order}

    OutgoingPulse -> do
      {m_ws, m_my_info, peers_order, peers_alive, peers_power} <- H.get

      -- find players who have not sent an update for some time:
      (Milliseconds timeNow) <- unInstant <$> liftEffect now
      let timeCutOff = timeNow - pulseTimeoutMs
      let deadPlayers = Map.filter (olderThan timeCutOff) peers_alive

      if Map.isEmpty deadPlayers then pure unit
        else do
          let deadPlayersArray = Set.toUnfoldable $ Map.keys deadPlayers
          -- liftEffect $ log $ "deadPlayersArray = " <> show deadPlayersArray

          -- tell Lobby and parent to remove these players:
          void $ H.query _lobby 111 $ H.tell (Lobby.Q_ClearPlayers deadPlayersArray)
          H.raise $ O_RemovePeers deadPlayersArray

          -- delete the old players from state:
          let peers_order2 = Array.filter (\k -> Map.member k deadPlayers) peers_order
          let peers_alive2 = Map.filterKeys (\k -> Map.member k deadPlayers) peers_alive
          let peers_power2 = Map.filterKeys (\k -> Map.member k deadPlayers) peers_power
          H.modify_ $ _ { peers_alive = peers_alive2, peers_power = peers_power2 }

          -- if I am the leader, tell others about the change of peer order:
          state <- H.get
          if not $ i_am_leader state then pure unit
            else do
              liftEffect $ broadcastToPeers m_ws {peers_order: peers_order2}

      -- are we in the game play stage?
      case m_my_info of
        Nothing -> pure unit
        Just {peerId} -> do
          liftEffect $ broadcastToPeers m_ws {pulse: peerId}

    IncomingPulse peerId -> do
      time <- liftEffect now
      H.modify_ \s -> s { peers_alive = Map.insert peerId time s.peers_alive }

    OutgoingStateChanges changes -> do
      {m_ws} <- H.get
      liftEffect $ broadcastToPeers m_ws {changes}

    IncomingStateChanges changes -> do
      H.raise $ O_StateChanges changes

olderThan :: Number -> Instant -> Boolean
olderThan timeCutOff time =
  let (Milliseconds timeMs) = unInstant time in
  timeMs < timeCutOff

broadcastToPeers :: forall t. EncodeJson t => Maybe WebSocket -> t -> Effect Unit
broadcastToPeers (Just ws) msg = 
  WS.sendString ws $ stringify $ encodeJson msg
broadcastToPeers _ _ = pure unit

measurePower :: Effect Number
measurePower = do
  Milliseconds startTime <- unInstant <$> now
  let _task = product (1..1000)
  Milliseconds endTime <- unInstant <$> now
  pure $ 1000.0 / (10.0 + endTime - startTime)
