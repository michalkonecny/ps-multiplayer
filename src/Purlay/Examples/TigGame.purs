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
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
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
import Purlay.Examples.TigGame.Global (GState, PlayerId, initGState, maxX, maxY, tickPeriod_ms, tigLobbySpec)
import Purlay.Examples.TigGame.PlayerPiece (Direction(..), ObjInfo, PlayerPiece)
import Purlay.Examples.TigGame.PlayerPiece as PlayerPiece
import Purlay.GameCanvas as GameCanvas
import Purlay.GameObject (unGO)
import Purlay.HalogenHelpers (periodicEmitter, subscribeToKeyDownUp)
import Purlay.Lobby as Lobby
import Purlay.WSConnector as WSConnector
import Web.Socket.WebSocket as WS
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

mainTigGame :: Effect Unit
mainTigGame = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type GameState = {
  m_connection :: Maybe {ws::WS.WebSocket, my_peerId::PeerId}
, gstate :: GState
, m_myPiece :: Maybe PlayerPiece
, otherPieces :: PlayerPieces
}

type PlayerPieces = Map.Map PeerId PlayerPiece

initialGameState :: GameState
initialGameState =  { 
  m_connection: Nothing
, gstate: initGState
, m_myPiece: Nothing
, otherPieces: Map.empty
}

updateGState :: (GState -> GState) -> GameState -> GameState
updateGState f s@{gstate: gs} = s {gstate = (f gs) }

data Action
  -- output from components:
  = FromWSConnector WSConnector.Output
  | FromLobby       Lobby.Output
  | FromCoordinator Coordinator.Output
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
    case Int.fromString name, PlayerPiece.fromJson value of
      Just peerId, Right playerPiece ->
        Right $ SetPlayer peerId playerPiece
      _,_ -> 
        Left $ "ignoring change: " <> show {name, value: stringify value}

actionToChanges :: Action -> Array StateChange
actionToChanges (SetPlayer player piece) = [{name: show player, value: (unGO piece).encode}]
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
, canvas      :: H.Slot (GameCanvas.Query GState ObjInfo PlayerPiece.Action) Action Int 
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
      let pieces = List.Cons myPiece $ Map.values otherPieces
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
      ae$ HH.slot _wsconnector _wsconnectorN WSConnector.component unit (Just <<< FromWSConnector)
  render {m_connection: Just {ws, my_peerId}, m_myPiece} =
    HH.div_ $ sb do
      ae$ HH.slot _coordinator _coordinatorN 
          Coordinator.component {ws, my_peerId} (Just <<< FromCoordinator)
      ae$ HH.br_
      case m_myPiece of
        Nothing ->
          ae$ HH.slot _lobby _lobbyN 
              Lobby.component {valuesSpec: tigLobbySpec, my_playerId: my_peerId} (Just <<< FromLobby)
        Just _ ->
          ae$ HH.slot _canvas _canvasN 
              (GameCanvas.component {my_peerId, initGState, width: maxX, height: maxY}) unit Just

  handleAction = case _ of
    -- WSConnector tells us we are connected:
    FromWSConnector (WSConnector.O_Connected ws) -> do
      my_peerId <- liftEffect $ randomInt 0 2000000000
      H.modify_ $ _ { m_connection = Just {ws, my_peerId} }
    -- Lobby tells us we can start playing:
    FromLobby (Lobby.O_Play values) -> do
      -- set my player to the state:
      let name = fromMaybe "?" $ Map.lookup "name" values
      {m_connection} <- H.get
      case m_connection of
        Just {my_peerId} -> do
          let playerPiece = PlayerPiece.new { playerId: my_peerId, name }
          H.modify_ $ _ { m_myPiece = Just playerPiece }
          -- force a tick now to sync with others asap:
          handleAction FrameTick
        _ -> pure unit

      -- start frame ticker:
      void $ H.subscribe $ periodicEmitter "FrameTick" tickPeriod_ms FrameTick

      -- subscribe to keyboard events:
      subscribeToKeyDownUp HandleKeyDown HandleKeyUp

    FromCoordinator (Coordinator.O_NewLeader _) -> pure unit
    FromCoordinator (Coordinator.O_PeerJoined _) -> do
      -- let them know about our player:
      {m_connection,m_myPiece} <- H.get
      case m_connection, m_myPiece of
        Just {my_peerId}, Just myPiece -> do
          broadcastAction $ SetPlayer my_peerId myPiece
        _, _ -> pure unit
    FromCoordinator (Coordinator.O_PeersGone peers) -> do
      H.modify_ \s -> s { otherPieces = Array.foldl (flip Map.delete) s.otherPieces peers }
      updateCanvas
      -- let Lobby know:
      void $ H.query _lobby _lobbyN $ H.tell (Lobby.Q_ClearPlayers peers)

    -- Coordinator relayes messages from peer players:
    FromCoordinator (Coordinator.O_StateChanges changes) -> do
      case changesToActions changes of
        Left err -> liftEffect $ log err
        Right actions -> sequence_ $ map handleAction actions

    SetPlayer peerId playerPiece -> do
      {m_connection,m_myPiece,gstate,otherPieces} <- H.get

      -- update and redraw piece
      H.modify_ $ \ s -> s { otherPieces = Map.insert peerId playerPiece s.otherPieces }
      updateCanvas

      -- have I joined the game?
      case m_connection, m_myPiece of
        Just {my_peerId}, Just myPiece -> do -- joined already

          -- check for collision
          let collisionCheckResult = (unGO myPiece).handleAction gstate (PlayerPiece.CheckCollidedWith playerPiece)
          case collisionCheckResult of
            { m_object: Just myPiece' } ->
                handleCollisionResult my_peerId peerId myPiece'
            _ -> pure unit      

        _, _ -> do --  not joined yet, let Lobby know of the player:
          let { name } = (unGO playerPiece).info
          let values = Map.singleton "name" name
          void $ H.query _lobby _lobbyN $ H.tell (Lobby.Q_NewPlayer peerId values)

    SetIt it -> do
      H.modify_ $ updateGState (_ { it = it, itActive = false } )
      updateCanvas

      -- am I it?
      {m_connection} <- H.get
      case m_connection of
        Just {my_peerId} | it == my_peerId -> do
          -- activate myself as "it" a bit later:
          liftAff $ delay (Milliseconds 1000.0) -- 1 second
          H.modify_ $ updateGState $ _ { itActive = true }
          -- updateCanvas
        _ -> pure unit

    -- control my movement:
    HandleKeyDown _sid ev -> do
      case KE.key ev of
        "ArrowLeft"  -> playerAction $ PlayerPiece.PushStart L
        "ArrowRight" -> playerAction $ PlayerPiece.PushStart R
        "ArrowUp"    -> playerAction $ PlayerPiece.PushStart U
        "ArrowDown"  -> playerAction $ PlayerPiece.PushStart D
        _ -> pure unit
    HandleKeyUp _sid ev -> do
      case KE.key ev of
        "ArrowLeft"  -> playerAction $ PlayerPiece.PushStop L
        "ArrowRight" -> playerAction $ PlayerPiece.PushStop R
        "ArrowUp"    -> playerAction $ PlayerPiece.PushStop U
        "ArrowDown"  -> playerAction $ PlayerPiece.PushStop D
        _ -> pure unit

    FrameTick -> do
      {m_connection,gstate:{it},otherPieces} <- H.get
      -- are we in the game play stage?
      case m_connection of
        Nothing -> pure unit
        Just {my_peerId} -> do
          -- update position and velocity:
          playerAction $ PlayerPiece.FrameTick
          
          -- check whether "it" exists and if not, who should become "it":
          let itGone = my_peerId /= it && (not $ Map.member it otherPieces)
          let iAmNewIt = case Map.findMin otherPieces of
                          Just {key: minPeer} -> itGone && my_peerId < minPeer
                          _ -> itGone
          when iAmNewIt do
            -- there is no "it" and we are the player with lowest number, thus we should be it!
            H.modify_ $ updateGState $ _ { it = my_peerId }
            updateCanvas
            broadcastAction $ SetIt my_peerId

  playerAction p_action = do
    {m_connection,m_myPiece,gstate,otherPieces} <- H.get
    case m_connection, m_myPiece of
      Just {my_peerId}, Just playerPiece1 -> do
        -- take the player action:
        let { m_object: m_newPlayerPiece } = (unGO playerPiece1).handleAction gstate p_action
        -- if my piece changed, take note of it:
        playerPiece2 <- case m_newPlayerPiece of
          Nothing -> pure playerPiece1
          Just newPlayerPiece -> do
            -- update my piece locally:
            H.modify_ $ _ {m_myPiece = Just newPlayerPiece }
            updateCanvas
            -- update others of my move:
            broadcastAction $ SetPlayer my_peerId newPlayerPiece
            pure newPlayerPiece

        -- check for a collision:
        let m_collisionResult = getCollision my_peerId gstate playerPiece2 otherPieces
          
        case m_collisionResult of
          Nothing -> pure unit
          Just (Tuple collidedPlayer playerPiece3) -> 
            handleCollisionResult my_peerId collidedPlayer playerPiece3
      _,_ -> pure unit

  handleCollisionResult my_peerId collidedPlayer playerPiece' =
    do -- collision ocurred, bouncing off another piece
    {gstate:{it,itActive}} <- H.get
    -- update my piece locally:
    H.modify_ $ _ {m_myPiece = Just playerPiece' }
    updateCanvas
    -- DO NOT send my new position to peers:
    -- broadcastAction $ SetPlayer my_peerId playerPiece3

    -- if I am it, tig them!
    when (my_peerId == it && itActive) do
      -- gotcha! update it locally:
      H.modify_ $ updateGState $ _ { it = collidedPlayer }
      updateCanvas
      -- and announce new "it":
      broadcastAction $ SetIt collidedPlayer

getCollision :: PeerId -> GState -> PlayerPiece -> PlayerPieces -> Maybe (Tuple PeerId PlayerPiece)
getCollision peer1 gstate object1 gameObjects =
  findCollision (Map.toUnfoldable gameObjects :: List _)
  where
  findCollision List.Nil = Nothing
  findCollision (List.Cons (Tuple id piece) rest) = 
    case (unGO object1).handleAction gstate (PlayerPiece.CheckCollidedWith piece) of
      { m_object: Just object2 } -> Just (Tuple id object2)
      _ -> findCollision rest
