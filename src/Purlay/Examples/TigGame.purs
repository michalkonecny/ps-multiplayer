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

import Control.Monad.Rec.Class (forever)
import Control.SequenceBuildMonad (ae, sb)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError, decodeJson, encodeJson, parseJson, printJsonDecodeError, stringify)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..), hush)
import Data.Int as Int
import Data.List (List, find)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set as Set
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Console (log)
import Effect.Exception (error)
import Effect.Now (now)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Query.EventSource as ES
import Halogen.Query.EventSource as ES.EventSource
import Halogen.VDom.Driver (runUI)
import Purlay.Coordinator (PeerId)
import Purlay.Coordinator as Coordinator
import Purlay.Drawable (draw)
import Purlay.EitherHelpers (mapLeft, (<|||>))
import Purlay.Examples.TigGame.GState (GState(..), initGState)
import Purlay.Examples.TigGame.PlayerPiece (PlayerPiece(..), newPlayerPiece)
import Purlay.GameCanvas as GameCanvas
import Purlay.GameObject (anyGameObject, bounceOff, gameObjectRecord, updateXYState)
import Purlay.HalogenHelpers (periodicEmitter, subscribeToKeyDownUp)
import Purlay.JsonHelpers (class Jsonable, AnyJsonable(..), anyJsonable)
import Purlay.Lobby (Output(..), Player)
import Purlay.Lobby as Lobby
import Purlay.MovingPoint (MovingPoint)
import Purlay.MovingPoint as MPt
import WSListener (setupWSListener)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WS
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET


mainTigGame :: Effect Unit
mainTigGame = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI rootComponent unit body

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

type Name = String

defaultName :: Name
defaultName = "😷"

initialMPt :: Player -> MovingPoint
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
  m_myPeer :: Maybe PeerId
-- , it :: PeerId
, gstate :: GState
, itActive :: Boolean
, m_myPiece :: Maybe PlayerPiece
, otherPieces :: PlayerPieces
-- , gobjs :: GameObjects
-- , gvars :: GVars
}

type PlayerPieces = Map.Map PeerId PlayerPiece

initialGameState :: GameState
initialGameState =  { 
  m_myPeer: Nothing
, gstate: initGState
, itActive: true
, m_myPiece: Nothing
, otherPieces: Map.empty
-- , gobjs: Map.empty
-- , gvars: Map.singleton "it" (encodeJson 0)
}

data Action
  = HandleCoordinator Coordinator.Output
  | SetPlayer Player PlayerPiece
  | SetIt Player
  | FrameTick
  | HandleKeyDown H.SubscriptionId KeyboardEvent
  | HandleKeyUp H.SubscriptionId KeyboardEvent

_coordinator :: SProxy "coordinator"
_coordinator = SProxy

_canvas :: SProxy "canvas"
_canvas = SProxy

-- type Slots = 
--   ( coordinator :: H.Slot Coordinator.Query Coordinator.Output Int
--   , canvas      :: H.Slot (GameCanvas.Query GState) Action Int )

-- passStateToCanvas :: forall output.
--   H.HalogenM GameState Action Slots output Aff Unit
-- passStateToCanvas = do
--   {m_myPeer, gstate, m_myPiece, otherPieces} <- H.get
--   case m_myPeer, m_myPiece of
--     Just _, Just myPiece -> do
--       let pieces = map anyGameObject $ List.Cons myPiece $ Map.values otherPieces
--       void $ H.query _canvas 11 $ H.tell (GameCanvas.Q_NewState gstate pieces)
--     _, _ -> pure unit

rootComponent :: forall input output query. H.Component HH.HTML query input output Aff
rootComponent =
  H.mkComponent
    { 
      initialState: \a -> initialGameState
    , render
    , eval: H.mkEval $ H.defaultEval 
      -- { handleAction = handleAction, initialize = Just Init }
      { handleAction = handleAction }
    }
  where
  -- render :: GameState -> H.ComponentHTML Action Slots Aff
  render {m_myPeer: Nothing} =
    HH.div_
      [
        HH.slot _coordinator 1 (Coordinator.component tigLobbySpec) unit (Just <<< HandleCoordinator)
      ]
  render {m_myPeer: Just peerId} =
    HH.p_
      [
        HH.slot _coordinator 1 (Coordinator.component tigLobbySpec) unit (Just <<< HandleCoordinator)
      , HH.br_
      , HH.slot _canvas 11 (GameCanvas.component {peerId, initGState, width: maxX, height: maxY}) unit Just
      ]

  tigLobbySpec = sb do
    ae$
      { key: "name"
      , maxLength: 5
      , description: "Player's name"
      , default: defaultName
      }

  handleAction = case _ of
    -- Coordinator tells us we can start playing:
    HandleCoordinator (Coordinator.O_Started {my_id, lobby_values}) -> do
      -- set my player to the state:
      let name = fromMaybe "?" $ Map.lookup "name" lobby_values
      let playerPiece = newPlayerPiece { peerId: my_id, xyState: initialMPt my_id, name, radius: playerRadius }
      H.modify_ \ s ->
        s { m_myPeer = Just my_id, m_myPiece = Just playerPiece }
      -- force a Pulse now to sync with others asap:
      handleAction FrameTick

      -- start frame ticker:
      void $ H.subscribe $ periodicEmitter "FrameTick" tickPeriod_ms FrameTick

      -- subscribe to keyboard events:
      subscribeToKeyDownUp HandleKeyDown HandleKeyUp

    _ -> pure unit -- TODO

--     -- messages from peer players:
--     HandleCoordinator (Coordinator.O_StateChanges) msg -> do
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

-- -- adapted from https://milesfrain.github.io/purescript-halogen/guide/04-Lifecycles-Subscriptions.html#implementing-a-timer
-- pulseTimer :: forall m. MonadAff m => ES.EventSource m Action
-- pulseTimer = ES.EventSource.affEventSource \emitter -> do
--   fiber <- Aff.forkAff $ forever do
--     Aff.delay $ Milliseconds pulsePeriodMs
--     ES.EventSource.emit emitter Pulse

--   pure $ ES.EventSource.Finalizer do
--     Aff.killFiber (error "Event source finalized") fiber

getCollision :: PeerId -> PlayerPiece -> PlayerPieces -> Maybe (Tuple PeerId PlayerPiece)
getCollision peer1 object1 gameObjects =
  removeJust $ find (\(Tuple _ m) -> isJust m) $ 
    (Map.toUnfoldable $ map (\go -> bounceOff go object1) $ 
      Map.filterKeys (_ /= peer1) gameObjects :: List _)
  where
  removeJust (Just (Tuple p (Just go))) = Just (Tuple p go)
  removeJust _ = Nothing

