{-|
    Module      :  TigGameSmooth
    Description :  A simple multiplayer game of tig
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

   A simple multiplayer game of tig
-}
module Purlay.Examples.TigGame
-- (mainTigGame) 
where

import Prelude

import Control.SequenceBuildMonad (ae, sb)
import Data.Argonaut (decodeJson, encodeJson, stringify)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.List (List, find)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, sequence_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Console (log)
import Effect.Random (randomInt)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Purlay.Coordinator (StateChange, PeerId)
import Purlay.Coordinator as Coordinator
import Purlay.Examples.TigGame.GState (GState(..), GState_record, initGState)
import Purlay.Examples.TigGame.PlayerPiece (PlayerPiece(..), newPlayerPiece)
import Purlay.GameCanvas as GameCanvas
import Purlay.GameObject (anyGameObject, bounceOff, updateXYState)
import Purlay.HalogenHelpers (periodicEmitter, subscribeToKeyDownUp)
import Purlay.Lobby as Lobby
import Purlay.MovingPoint (MovingPoint)
import Purlay.MovingPoint as MPt
import Purlay.WSConnector as WSConnector
import Web.Socket.WebSocket as WS
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE


mainTigGame :: Effect Unit
mainTigGame = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

tickPeriod_ms :: Number
tickPeriod_ms = 50.0

maxX :: Number
maxX = 800.0
maxY :: Number
maxY = 800.0

maxSpeed :: Number
maxSpeed = 20.0

slowDownRatio :: Number
slowDownRatio = 0.9

speedIncrement :: Number
speedIncrement = 2.0

playerRadius :: Number
playerRadius = 25.0

tigLobbySpec :: Lobby.ValuesSpec
tigLobbySpec = sb do
  ae$
    { key: "name"
    , maxLength: 5
    , description: "Player's name"
    , default: defaultName
    }

type Name = String

defaultName :: Name
defaultName = "😷"

type PlayerId = Int

initialMPt :: PlayerId -> MovingPoint
initialMPt player = 
  MPt.constrainPosWrapAround { minX: 0.0, maxX, minY: 0.0, maxY } $
  { pos:
    { x: playerN*107.0, 
      y: playerN*107.0 }
  , velo: { x: 0.0, y: 0.0 }
  , accell: { x: 0.0, y: 0.0 }
  }
  where
  playerN = Int.toNumber player

type GameState = {
  m_connection :: Maybe {ws::WS.WebSocket, my_peerId::PeerId}
, gstate :: GState
, itActive :: Boolean
, m_myPiece :: Maybe PlayerPiece
, otherPieces :: PlayerPieces
-- , gobjs :: GameObjects
}

type PlayerPieces = Map.Map PeerId PlayerPiece

initialGameState :: GameState
initialGameState =  { 
  m_connection: Nothing
, gstate: initGState
, itActive: true
, m_myPiece: Nothing
, otherPieces: Map.empty
-- , gobjs: Map.empty
-- , gvars: Map.singleton "it" (encodeJson 0)
}

updateGState :: (GState_record -> GState_record) -> GameState -> GameState
updateGState f s@{gstate: GState gr} = s {gstate = GState (f gr) }

data Action
  -- output from components:
  = HandleWSConnector WSConnector.Output
  | HandleLobby       Lobby.Output
  | HandleCoordinator Coordinator.Output
  -- messages from peers via Coordinator:
  | SetPlayer PlayerId PlayerPiece
  | SetIt PlayerId
  -- internal:
  | FrameTick
  | HandleKeyDown H.SubscriptionId KeyboardEvent
  | HandleKeyUp   H.SubscriptionId KeyboardEvent

-- boilerplate for broadcasting and receiving actions via Coordinator:

changesToActions :: Array StateChange -> Either String (Array Action)
changesToActions changes = sequence $ map doOne changes
  where
  doOne {name: "it", value} =
    case decodeJson value of
      Right it -> Right $ SetIt it
      Left err -> Left $ "failed to parse `it`: " <> show err
  doOne {name, value} =
    case Int.fromString name, decodeJson value of
      Just peerId, Right playerPiece ->
        Right $ SetPlayer peerId playerPiece
      _,_ -> 
        Left $ "ignoring change: " <> show {name, value: stringify value}

actionToChanges :: Action -> Array StateChange
actionToChanges (SetPlayer player piece) = [{name: show player, value: encodeJson piece}]
actionToChanges (SetIt it) = [{name: "it", value: encodeJson it}]
actionToChanges _ = []

-- wiring for sub-components:

_wsconnector :: SProxy "wsconnector"
_wsconnector = SProxy
_wsconnectorN :: Int
_wsconnectorN = 0

_coordinator :: SProxy "coordinator"
_coordinator = SProxy
_coordinatorN :: Int
_coordinatorN = 1

_lobby :: SProxy "lobby"
_lobby = SProxy
_lobbyN :: Int
_lobbyN = 2

_canvas :: SProxy "canvas"
_canvas = SProxy
_canvasN :: Int
_canvasN = 3

type Slots = ( 
  wsconnector :: forall query . H.Slot query WSConnector.Output Int
, coordinator :: H.Slot Coordinator.Query Coordinator.Output Int
, lobby       :: H.Slot Lobby.Query Lobby.Output Int
, canvas      :: H.Slot (GameCanvas.Query GState) Action Int 
)

broadcastAction :: forall output. Action -> H.HalogenM GameState Action Slots output Aff Unit
broadcastAction action =
  void $ H.query _coordinator _coordinatorN $ H.tell $
      Coordinator.Q_StateChanges $ actionToChanges action

updateCanvas :: forall output. H.HalogenM GameState Action Slots output Aff Unit
updateCanvas = do
  {gstate, m_myPiece, otherPieces} <- H.get
  case m_myPiece of
    Just myPiece -> do
      let pieces = map anyGameObject $ List.Cons myPiece $ Map.values otherPieces
      void $ H.query _canvas _canvasN $ H.tell (GameCanvas.Q_NewState gstate pieces)
    _ -> pure unit

component :: forall input output query. H.Component HH.HTML query input output Aff
component =
  H.mkComponent
    { 
      initialState: \a -> initialGameState
    , render
    , eval: H.mkEval $ H.defaultEval 
      -- { handleAction = handleAction, initialize = Just Init }
      { handleAction = handleAction }
    }
  where
  render {m_connection: Nothing} =
    HH.div_ $ sb do
      ae$ HH.slot _wsconnector _wsconnectorN WSConnector.component unit (Just <<< HandleWSConnector)
  render {m_connection: Just {ws, my_peerId}, m_myPiece} =
    HH.div_ $ sb do
      ae$ HH.slot _coordinator _coordinatorN 
          Coordinator.component {ws, my_peerId} (Just <<< HandleCoordinator)
      ae$ HH.br_
      case m_myPiece of
        Nothing ->
          ae$ HH.slot _lobby _lobbyN 
              Lobby.component {valuesSpec: tigLobbySpec, my_playerId: my_peerId} (Just <<< HandleLobby)
        Just _ ->
          ae$ HH.slot _canvas _canvasN 
              (GameCanvas.component {my_peerId, initGState, width: maxX, height: maxY}) unit Just

  handleAction = case _ of
    -- WSConnector tells us we are connected:
    HandleWSConnector (WSConnector.O_Connected ws) -> do
      my_peerId <- liftEffect $ randomInt 0 2000000000
      H.modify_ $ _ { m_connection = Just {ws, my_peerId} }
    -- Lobby tells us we can start playing:
    HandleLobby (Lobby.O_Play values) -> do
      -- set my player to the state:
      let name = fromMaybe "?" $ Map.lookup "name" values
      {m_connection} <- H.get
      case m_connection of
        Just {my_peerId} -> do
          let playerPiece = newPlayerPiece 
                { peerId: my_peerId, xyState: initialMPt my_peerId, name, radius: playerRadius }
          H.modify_ $ _ { m_myPiece = Just playerPiece }
          -- force a tick now to sync with others asap:
          handleAction FrameTick
        _ -> pure unit

      -- start frame ticker:
      void $ H.subscribe $ periodicEmitter "FrameTick" tickPeriod_ms FrameTick

      -- subscribe to keyboard events:
      subscribeToKeyDownUp HandleKeyDown HandleKeyUp

    HandleCoordinator (Coordinator.O_NewLeader _) -> pure unit
    HandleCoordinator (Coordinator.O_PeerJoined _) -> pure unit
    HandleCoordinator (Coordinator.O_PeersGone peers) -> do
      H.modify_ \s -> s { otherPieces = Array.foldl (flip Map.delete) s.otherPieces peers }
      updateCanvas
      -- let Lobby know:
      void $ H.query _lobby _lobbyN $ H.tell (Lobby.Q_ClearPlayers peers)

    -- Coordinator relayes messages from peer players:
    HandleCoordinator (Coordinator.O_StateChanges changes) -> do
      case changesToActions changes of
        Left err -> liftEffect $ log err
        Right actions -> sequence_ $ map handleAction actions

    SetPlayer peerId playerPiece@(PlayerPiece {name}) -> do
      H.modify_ $ \ s -> s { otherPieces = Map.insert peerId playerPiece s.otherPieces }
      updateCanvas
      -- let Lobby know:
      let values = Map.singleton "name" name
      void $ H.query _lobby _lobbyN $ H.tell (Lobby.Q_NewPlayer peerId values)

    SetIt it -> do
      H.modify_ $ updateGState (_ { it = it } ) <<< _ { itActive = false }
      updateCanvas

      -- am I it?
      {m_connection} <- H.get
      case m_connection of
        Just {my_peerId} | it == my_peerId -> do
          -- activate myself as "it" a bit later:
          liftAff $ delay (Milliseconds 1000.0) -- 1 second
          H.modify_ $ _ { itActive = true }
          -- updateCanvas
        _ -> pure unit

    -- control my movement:
    HandleKeyDown _sid ev -> do
      case KE.key ev of
        "ArrowLeft"  -> handleMoveBy (MPt.setAccelX (- speedIncrement))
        "ArrowRight" -> handleMoveBy (MPt.setAccelX speedIncrement)
        "ArrowUp"    -> handleMoveBy (MPt.setAccelY (- speedIncrement))
        "ArrowDown"  -> handleMoveBy (MPt.setAccelY speedIncrement)
        _ -> pure unit
    HandleKeyUp _sid ev -> do
      case KE.key ev of
        "ArrowLeft"  -> handleMoveBy (MPt.resetAccelX (- speedIncrement))
        "ArrowRight" -> handleMoveBy (MPt.resetAccelX speedIncrement)
        "ArrowUp"    -> handleMoveBy (MPt.resetAccelY (- speedIncrement))
        "ArrowDown"  -> handleMoveBy (MPt.resetAccelY speedIncrement)
        _ -> pure unit

    FrameTick -> do
      {m_connection,m_myPiece,itActive,gstate:GState {it},otherPieces} <- H.get
      -- are we in the game play stage?
      case m_connection of
        Nothing -> pure unit
        Just {my_peerId} -> do
          -- update position and velocity:
          handleMoveBy $
            MPt.move {slowDownRatio}
            >>> MPt.constrainSpeed {maxSpeed} 
            >>> MPt.constrainPosWrapAround {minX:0.0, maxX, minY: 0.0, maxY}
          
          -- check whether "it" exists and if not, who should become "it":
          let itGone = my_peerId /= it && (not $ Map.member it otherPieces)
          let iAmNewIt = case Map.findMin otherPieces of
                          Just {key: minPeer} -> itGone && my_peerId < minPeer
                          _ -> itGone
          when iAmNewIt do
            -- there is no "it" and we are the player with lowerst number, thus we should be it!
            H.modify_ $ updateGState $ _ { it = my_peerId }
            broadcastAction $ SetIt my_peerId

  handleMoveBy move = do
    {m_connection,m_myPiece,itActive,gstate:GState {it},otherPieces} <- H.get
    case m_connection, m_myPiece of
      Just {my_peerId}, Just playerPiece -> do
        -- make the move locally:
        let newPlayerPiece = updateXYState (\r -> move r.xyState) playerPiece
        H.modify_ $ _ {m_myPiece = Just newPlayerPiece }
        updateCanvas

        -- send my new position to peers:
        broadcastAction $ SetPlayer my_peerId newPlayerPiece

        -- check for a collision:
        case getCollision my_peerId newPlayerPiece otherPieces of
          Nothing -> pure unit
          Just (Tuple collidedPlayer newPlayerPiece2) -> do -- collision occurred, bounced off another player
              -- change my movement due to the bounce:
              H.modify_ $ _ {m_myPiece = Just newPlayerPiece2 }
              updateCanvas

            -- if I am it, tig them!
              when (my_peerId == it && itActive) do
                -- gotcha! update it locally:
                H.modify_ $ updateGState $ _ { it = collidedPlayer }
                updateCanvas
                -- and announce new "it":
                broadcastAction $ SetIt collidedPlayer
      _,_ -> pure unit

getCollision :: PeerId -> PlayerPiece -> PlayerPieces -> Maybe (Tuple PeerId PlayerPiece)
getCollision peer1 object1 gameObjects =
  removeJust $ find (\(Tuple _ m) -> isJust m) $ 
    (Map.toUnfoldable $ map (\go -> bounceOff go object1) $ 
      Map.filterKeys (_ /= peer1) gameObjects :: List _)
  where
  removeJust (Just (Tuple p (Just go))) = Just (Tuple p go)
  removeJust _ = Nothing

