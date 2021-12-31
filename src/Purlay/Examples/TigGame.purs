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
import Purlay.Examples.TigGame.Global (Direction(..)) as Direction
import Purlay.Examples.TigGame.Global (ObjAction(..)) as ObjAction
import Purlay.Examples.TigGame.Global (ObjAction, ObjInfo, PlayerId, TigObject, TigState, initTigState, maxX, maxY, tickPeriod_ms)
import Purlay.Examples.TigGame.PlayerPiece as PlayerPiece
import Purlay.GameCanvas as GameCanvas
import Purlay.GameObject (unGO)
import Purlay.HalogenHelpers (subscribePeriodicAction, subscribeToKeyDownUp)
import Purlay.Lobby as Lobby
import Purlay.WSConnector as WSConnector
import Type.Proxy (Proxy(..))
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
, i_am_leader :: Boolean
, gstate :: TigState
, m_myPiece :: Maybe TigObject
, otherPieces :: PlayerPieces
-- , balls :: Balls
}

type PlayerPieces = Map.Map PlayerId TigObject
-- type Balls = Map.Map BallId Ball

initialGameState :: GameState
initialGameState =  { 
  m_connection: Nothing
, i_am_leader: false
, gstate: initTigState
, m_myPiece: Nothing
, otherPieces: Map.empty
-- , balls: Map.empty
}

updateTigState :: (TigState -> TigState) -> GameState -> GameState
updateTigState f s@{gstate: gs} = s {gstate = (f gs) }

data Action
  -- output from components:
  = FromWSConnector WSConnector.Output
  | FromLobby       Lobby.Output
  | FromCoordinator Coordinator.Output
  -- messages from peers via Coordinator:
  | SetPlayer PlayerId TigObject
  | SetIt PlayerId
  -- | SetBall BallId Ball
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
  -- doOne {name, value} | "ball" == (StringCU.take 4 name) =
  --   case Int.fromString (StringCU.drop 4 name), Ball.fromJson value of
  --     Just ballId, Right ball ->
  --       Right $ SetBall ballId ball
  --     _,_ -> 
  --       Left $ "ignoring change: " <> show {name, value: stringify value}
  doOne {name, value} =
    case Int.fromString name, PlayerPiece.fromJson value of
      Just peerId, Right playerPiece ->
        Right $ SetPlayer peerId playerPiece
      _,_ -> 
        Left $ "ignoring change: " <> show {name, value: stringify value}

actionToChanges :: Action -> Array StateChange
actionToChanges (SetPlayer player piece) = [{name: show player, value: (unGO piece).encode}]
-- actionToChanges (SetBall i ball) = [{name: "ball" <> (show i), value: (unGO ball).encode}]
actionToChanges (SetIt it) = [{name: "it", value: encodeJson it}]
actionToChanges _ = []

-- wiring for sub-components:
_wsconnector = Proxy :: Proxy "wsconnector"
_wsconnectorN = 0 :: Int

_coordinator = Proxy :: Proxy "coordinator"
_coordinatorN = 1 :: Int

_lobby = Proxy :: Proxy "lobby"
_lobbyN = 2 :: Int

_canvas = Proxy :: Proxy "canvas"
_canvasN = 3 :: Int

type Slots = ( 
  wsconnector :: forall query . H.Slot query WSConnector.Output Int
, coordinator :: H.Slot Coordinator.Query Coordinator.Output Int
, lobby       :: H.Slot Lobby.Query Lobby.Output Int
, canvas      :: H.Slot (GameCanvas.Query TigState ObjInfo ObjAction) Action Int 
)

broadcastAction :: forall output. Action -> H.HalogenM GameState Action Slots output Aff Unit
broadcastAction action =
  void $ H.tell _coordinator _coordinatorN $
      Coordinator.Q_StateChanges $ actionToChanges action

updateCanvas :: forall output. H.HalogenM GameState Action Slots output Aff Unit
updateCanvas = do
  {gstate, m_myPiece, otherPieces} <- H.get
  case m_myPiece of
    Just myPiece -> do
      let pieces = List.Cons myPiece $ Map.values otherPieces
      void $ H.tell _canvas _canvasN $ GameCanvas.Q_NewState gstate pieces
    _ -> pure unit

tigLobbySpec :: Lobby.ValuesSpec
tigLobbySpec = sb do
  ae$
    { key: "name"
    , maxLength: 5
    , description: "Player's name"
    , default: PlayerPiece.defaultName
    }

component :: forall input output query. H.Component query input output Aff
component =
  H.mkComponent
    { 
      initialState: \_ -> initialGameState
    , render
    , eval: H.mkEval $ H.defaultEval 
      -- { handleAction = handleAction, initialize = Just Init }
      { handleAction = handleAction }
    }
  where
  render {m_connection: Nothing} =
    HH.div_ $ sb do
      ae$ HH.slot _wsconnector _wsconnectorN WSConnector.component unit FromWSConnector
  render {m_connection: Just {ws, my_peerId}, i_am_leader, m_myPiece} =
    HH.div_ $ sb do
      ae$ HH.slot _coordinator _coordinatorN 
          Coordinator.component {ws, my_peerId} FromCoordinator
      -- ae$ HH.br_
      ae$ if i_am_leader then HH.hr_ else HH.br_
      case m_myPiece of
        Nothing ->
          ae$ HH.slot _lobby _lobbyN 
              Lobby.component {valuesSpec: tigLobbySpec, my_playerId: my_peerId} FromLobby
        Just _ ->
          ae$ HH.slot _canvas _canvasN 
              (GameCanvas.component {my_peerId, initGState: initTigState, width: maxX, height: maxY}) unit identity

  handleAction :: Action -> H.HalogenM GameState Action Slots output Aff Unit
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
          let playerPiece = PlayerPiece.new { m_playerId: Just my_peerId, name }
          H.modify_ $ _ { m_myPiece = Just playerPiece }
          -- force a tick now to sync with others asap:
          handleAction FrameTick
        _ -> pure unit

      -- start frame ticker:
      subscribePeriodicAction (Milliseconds tickPeriod_ms) FrameTick

      -- subscribe to keyboard events:
      subscribeToKeyDownUp HandleKeyDown HandleKeyUp

    FromCoordinator (Coordinator.O_NewLeader leaderId) -> do
      {m_connection} <- H.get
      case m_connection of
        Just { my_peerId } ->
          H.modify_ $ _ { i_am_leader = leaderId == my_peerId }
        Nothing -> pure unit
    FromCoordinator (Coordinator.O_PeerJoined _) -> do
      -- let them know about our player:
      {m_connection,m_myPiece} <- H.get
      case m_connection, m_myPiece of
        Just {my_peerId}, Just myPiece -> do
          broadcastAction $ SetPlayer my_peerId myPiece
        _, _ -> pure unit
      -- -- if we are leading, let them also know of the balls:
      -- when i_am_leader do
      --   sequence_ $ map (\(Tuple i b) -> broadcastAction $ SetBall i b) 
      --     (Map.toUnfoldable balls :: Array _)
    FromCoordinator (Coordinator.O_PeersGone peers) -> do
      H.modify_ \s -> s { otherPieces = Array.foldl (flip Map.delete) s.otherPieces peers }
      updateCanvas
      -- let Lobby know:
      void $ H.tell _lobby _lobbyN $ Lobby.Q_ClearPlayers peers

    -- Coordinator relayes messages from peer players:
    FromCoordinator (Coordinator.O_StateChanges changes) -> do
      case changesToActions changes of
        Left err -> liftEffect $ log err
        Right actions -> sequence_ $ map handleAction actions

    -- SetBall ballId ball -> do
    --   {m_connection,m_myPiece,gstate} <- H.get
    --   -- update and redraw ball
    --   H.modify_ $ \ s -> s { balls = Map.insert ballId ball s.balls }
    --   updateCanvas

    --   -- have I joined the game?
    --   case m_connection, m_myPiece of
    --     Just _, Just myPiece -> do -- joined already

    --       -- check for collision with my piece:
    --       let collisionCheckResult = 
    --             (unGO myPiece).handleAction gstate 
    --               (PlayerPiece.CheckCollidedWith (unGO ball).movingShape)
    --       case collisionCheckResult of
    --         { m_object: Just myPiece' } -> do
    --           H.modify_ $ _ {m_myPiece = Just myPiece' }
    --           updateCanvas
    --         _ -> pure unit  

    --     _, _ -> pure unit --  not joined yet, no need to do anything

    SetPlayer peerId playerPiece -> do
      {m_connection,m_myPiece,gstate} <- H.get

      -- update and redraw piece
      H.modify_ $ \ s -> s { otherPieces = Map.insert peerId playerPiece s.otherPieces }
      updateCanvas

      -- have I joined the game?
      case m_connection, m_myPiece of
        Just {my_peerId}, Just myPiece -> do -- joined already

          -- check for collision
          let collisionCheckResult = 
                (unGO myPiece).handleAction gstate 
                  (ObjAction.CheckCollidedWith (unGO playerPiece).movingShape)
          case collisionCheckResult of
            { m_object: Just myPiece' } ->
                handleCollisionResult my_peerId peerId myPiece'
            _ -> pure unit      

        _, _ -> do --  not joined yet, let Lobby know of the player:
          let { name } = (unGO playerPiece).info
          let values = Map.singleton "name" name
          void $ H.tell _lobby _lobbyN $ Lobby.Q_NewPlayer peerId values

    SetIt it -> do
      H.modify_ $ updateTigState (_ { it = it, itActive = false } )
      updateCanvas

      -- am I it?
      {m_connection} <- H.get
      case m_connection of
        Just {my_peerId} | it == my_peerId -> do
          -- activate myself as "it" a bit later:
          liftAff $ delay (Milliseconds 1000.0) -- 1 second
          H.modify_ $ updateTigState $ _ { itActive = true }
          -- updateCanvas
        _ -> pure unit

    -- control my movement:
    HandleKeyDown _sid ev -> do
      case KE.key ev of
        "ArrowLeft"  -> playerAction $ ObjAction.PushStart Direction.L
        "ArrowRight" -> playerAction $ ObjAction.PushStart Direction.R
        "ArrowUp"    -> playerAction $ ObjAction.PushStart Direction.U
        "ArrowDown"  -> playerAction $ ObjAction.PushStart Direction.D
        _ -> pure unit
    HandleKeyUp _sid ev -> do
      case KE.key ev of
        "ArrowLeft"  -> playerAction $ ObjAction.PushStop Direction.L
        "ArrowRight" -> playerAction $ ObjAction.PushStop Direction.R
        "ArrowUp"    -> playerAction $ ObjAction.PushStop Direction.U
        "ArrowDown"  -> playerAction $ ObjAction.PushStop Direction.D
        _ -> pure unit

    FrameTick -> do
      {m_connection,gstate:{it},otherPieces, i_am_leader} <- H.get
      -- are we in the game play stage?
      case m_connection of
        Nothing -> pure unit
        Just {my_peerId} -> do
          -- update position and velocity:
          playerAction $ ObjAction.FrameTick

          when i_am_leader do -- TODO
            -- add any missing balls
            -- updte ball position and velocity
            -- for each ball, identify collisions and deal with them
            pure unit
          
          -- check whether "it" exists and if not, who should become "it":
          let itGone = my_peerId /= it && (not $ Map.member it otherPieces)
          let iAmNewIt = case Map.findMin otherPieces of
                          Just {key: minPeer} -> itGone && my_peerId < minPeer
                          _ -> itGone
          when iAmNewIt do
            -- there is no "it" and we are the player with lowest number, thus we should be it!
            H.modify_ $ updateTigState $ _ { it = my_peerId }
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
      H.modify_ $ updateTigState $ _ { it = collidedPlayer }
      updateCanvas
      -- and announce new "it":
      broadcastAction $ SetIt collidedPlayer

getCollision :: PeerId -> TigState -> TigObject -> PlayerPieces -> Maybe (Tuple PeerId TigObject)
getCollision _peer1 gstate object1 gameObjects =
  findCollision (Map.toUnfoldable gameObjects :: List _)
  where
  findCollision List.Nil = Nothing
  findCollision (List.Cons (Tuple id piece) rest) = 
    case (unGO object1).handleAction gstate (ObjAction.CheckCollidedWith (unGO piece).movingShape) of
      { m_object: Just object2 } -> Just (Tuple id object2)
      _ -> findCollision rest
