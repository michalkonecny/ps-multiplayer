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
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Console (log)
import Effect.Now (now)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Purlay.EitherHelpers (mapLeft, (<||>), (<|||>))
import Purlay.HalogenHelpers (subscribeAffNotifier, subscribePeriodicAction)
import WSListener (setupWSListener)
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WS

pulsePeriod_ms :: Number
pulsePeriod_ms = 500.0

pulseTimeoutMs :: Number
pulseTimeoutMs = 2000.0

checkPowerPeriod_ms :: Number
checkPowerPeriod_ms = 5000.0

type PeerId = Int

type Input = {my_peerId :: PeerId, ws :: WS.WebSocket}

data Output 
  = O_StateChanges StateChanges
  | O_PeerJoined PeerId
  | O_PeersGone (Array PeerId)
  | O_NewLeader PeerId

data Query a
  = Q_StateChanges StateChanges a

type StateChange = { name :: String, value :: Json }
type StateChanges = Array StateChange

type State = {
  my_peerId       :: PeerId
, ws              :: WS.WebSocket
, peers_alive     :: Map.Map PeerId Instant
, peers_order     :: Array PeerId
, peers_power     :: Map.Map PeerId PowerMeasurement
}

type PowerMeasurement = Number -- the larger, the more powerful

initialState :: Input -> State
initialState  {ws, my_peerId} = {
  my_peerId
, ws
, peers_order:     []
, peers_alive:     Map.empty
, peers_power:     Map.empty
}

data Action
  = Init
  | OutgoingPulse
  | MeasurePower
  | CheckPeerOrder
  | OutgoingStateChanges StateChanges
  | ReceiveMessageFromPeer String
  -- the following come from peers, via decoding the above String:
  | IncomingPulse PeerId
  | IncomingPeersOrder (Array PeerId)
  | IncomingPowerMeasurement {peerId::PeerId, power::PowerMeasurement}
  | IncomingStateChanges StateChanges

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
    parseNewPowerMeasurementAndValues json 
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
  parseNewPowerMeasurementAndValues json = do
    ({peerId, power} :: 
      {peerId :: PeerId, power :: PowerMeasurement}) <- decodeJson json
    pure (IncomingPowerMeasurement {peerId, power})

  describeErr :: forall b.String -> Either JsonDecodeError b -> Either String b
  describeErr s = mapLeft (\ err -> s <> (printJsonDecodeError err))
  describeErrs :: forall b.String -> Either (Array JsonDecodeError) b -> Either String b
  describeErrs s = mapLeft (\ errs -> s <> String.joinWith "\n" (map printJsonDecodeError errs))

component :: H.Component Query Input Output Aff
component =
  H.mkComponent
    { 
      initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      -- { handleAction = handleAction, initialize = Just Init }
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Init }
    }
  where
  i_am_leader { my_peerId, peers_order } = Array.head peers_order == Just my_peerId

  render {my_peerId, peers_power, peers_order} = 
    HH.div_ [ HH.text $ show {my_peerId, peers_order, peers_power} ]

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    Q_StateChanges changes _ -> do
      handleAction $ OutgoingStateChanges changes
      pure Nothing

  handleAction = case _ of
    Init -> do
      {ws} <- H.get
      -- start listening to the websocket:
      subscribeAffNotifier $ \listener ->
        setupWSListener ws (\msg -> H.liftEffect $ HS.notify listener (ReceiveMessageFromPeer msg))
      -- start periodic emitters for Pulse and MeasurePower actions:
      subscribePeriodicAction (Milliseconds pulsePeriod_ms) OutgoingPulse
      subscribePeriodicAction (Milliseconds checkPowerPeriod_ms) MeasurePower

      handleAction MeasurePower

    MeasurePower -> do
      {ws, my_peerId} <- H.get
      -- liftEffect $ log "MeasurePower"
      power <- liftEffect measurePower
      liftEffect $ broadcastToPeers ws {peerId:my_peerId, power}

      H.modify_ \s ->  s { peers_power = Map.insert my_peerId power s.peers_power }
      handleAction CheckPeerOrder

    CheckPeerOrder -> do
      -- liftEffect $ log "CheckPeerOrder"
      state@{ws, peers_power, peers_order: peers_order1} <- H.get
      when (i_am_leader state || null peers_order1) do
        
        let peers_order2 = map fst $ Array.sortBy compSnd $ Map.toUnfoldableUnordered peers_power
        -- liftEffect $ log $ "CheckPeerOrder: peers_order2 = " <> show peers_order2
        H.modify_ $ _ { peers_order = peers_order2 }
        when (i_am_leader state) do
          liftEffect $ broadcastToPeers ws {peers_order: peers_order2}
          case Array.head peers_order2 of
            Just leader -> H.raise $ O_NewLeader leader
            _ -> pure unit

    -- messages from peers:
    ReceiveMessageFromPeer msg -> do
      case messageToAction msg of
        Left err -> liftEffect $ log err
        Right action -> handleAction action

    IncomingPowerMeasurement {peerId, power} -> do
      {peers_power} <- H.get
      when (not $ Map.member peerId peers_power) do
        H.raise $ O_PeerJoined peerId
      H.modify_ $ _
        { peers_power = Map.insert peerId power peers_power }

    IncomingPeersOrder peers_order -> do
      H.modify_ $ _ {peers_order = peers_order}
      case Array.head peers_order of
        Just leader -> H.raise $ O_NewLeader leader
        _ -> pure unit

    IncomingPulse peerId -> do
      time <- liftEffect now
      H.modify_ \s -> s { peers_alive = Map.insert peerId time s.peers_alive }

    OutgoingPulse -> do
      {ws, my_peerId, peers_order, peers_alive, peers_power} <- H.get

      liftEffect $ broadcastToPeers ws {pulse: my_peerId}

      -- find players who have not sent an update for some time:
      (Milliseconds timeNow) <- unInstant <$> liftEffect now
      let timeCutOff = timeNow - pulseTimeoutMs
      let deadPlayers = Map.filter (olderThan timeCutOff) peers_alive

      when (not $ Map.isEmpty deadPlayers) do
        let deadPlayersArray = Set.toUnfoldable $ Map.keys deadPlayers
        liftEffect $ log $ "deadPlayersArray = " <> show deadPlayersArray

        -- tell parent about the dead players:
        H.raise $ O_PeersGone deadPlayersArray

        -- delete the dead players from state:
        let isAlive = not <<< (_ `Map.member` deadPlayers)
        let peers_order2 = Array.filter isAlive peers_order
        let peers_alive2 = Map.filterKeys isAlive peers_alive
        let peers_power2 = Map.filterKeys isAlive peers_power
        H.modify_ $ _ 
          { peers_alive  = peers_alive2
          , peers_power  = peers_power2
          , peers_order  = peers_order2}

        -- if I am the leader, tell others about the change of peer order:
        state <- H.get
        when (i_am_leader state) do
          liftEffect $ broadcastToPeers ws {peers_order: peers_order2}

    OutgoingStateChanges changes -> do
      {ws} <- H.get
      liftEffect $ broadcastToPeers ws {changes}

    IncomingStateChanges changes -> do
      H.raise $ O_StateChanges changes

olderThan :: Number -> Instant -> Boolean
olderThan timeCutOff time =
  let (Milliseconds timeMs) = unInstant time in
  timeMs < timeCutOff

broadcastToPeers :: forall t. EncodeJson t => WebSocket -> t -> Effect Unit
broadcastToPeers ws msg = 
  WS.sendString ws $ stringify $ encodeJson msg

measurePower :: Effect Number
measurePower = do
  Milliseconds startTime <- unInstant <$> now
  let _task = product (1..1000)
  Milliseconds endTime <- unInstant <$> now
  pure $ 1000.0 / (10.0 + endTime - startTime)

compSnd :: forall t. Tuple t Number -> Tuple t Number -> Ordering
compSnd (Tuple _ p1) (Tuple _ p2) = compare (p1::Number) p2