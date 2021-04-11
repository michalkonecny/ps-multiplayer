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
module TigGameSmooth(mainTigGame) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Control.SequenceBuildMonad (ae, sb)
import Data.Argonaut (JsonDecodeError, decodeJson, encodeJson, parseJson, printJsonDecodeError, stringify)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..))
import Data.Int as Int
import Data.List.Lazy (List, find)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Set as Set
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Exception (error)
import Effect.Now (now)
import EitherHelpers (mapLeft, (<|||>))
import Graphics.Canvas (TextAlign(..))
import Graphics.Canvas as Canvas
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Query.EventSource as ES
import Halogen.Query.EventSource as ES.EventSource
import Halogen.VDom.Driver (runUI)
import Lobby (Output(..), Player)
import Lobby as Lobby
import Math (pi)
import MovingPoint (MovingPoint, constrainLocation)
import MovingPoint as MPt
import WSListener (setupWSListener)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.Socket.WebSocket as WS
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

mainTigGame :: Effect Unit
mainTigGame = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI rootComponent unit body

pulsePeriodMs :: Number
pulsePeriodMs = 50.0

pulseTimeoutMs :: Number
pulseTimeoutMs = 1000.0

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

type Shape = String

defaultShape :: Shape
defaultShape = "😷"

type MovingShape = { center :: MovingPoint, shape :: Shape, radius :: Number }

shapesAreColliding :: MovingShape -> MovingShape -> Boolean
shapesAreColliding 
    {center: {pos: {x:x1,y:y1}}, radius: r1}
    {center: {pos: {x:x2,y:y2}}, radius: r2} =
  (sqr $ x1-x2) + (sqr $ y1-y2) <= (sqr $ r1+r2)
  where
  sqr a = a * a

initialMPt :: Player -> MovingPoint
initialMPt player = 
  constrainLocation maxX maxY $
  { pos:
    { x: playerN*107.0, 
      y: playerN*107.0 }
  , velo: { x: 0.0, y: 0.0 }
  , accell: { x: 0.0, y: 0.0 }
  }
  where
  playerN = Int.toNumber player

type State = {
  m_ws :: Maybe WS.WebSocket
, m_myPlayer :: Maybe Player
, gameState :: GameState
}

updateGameState :: (GameState -> GameState) -> State -> State
updateGameState fn state@{gameState} = state {gameState = fn gameState}

initialState :: State
initialState =  
  { m_ws: Nothing
  , m_myPlayer: Nothing 
  , gameState: initialGameState
  }

type MovingShapeTime = { movingShape :: MovingShape, time :: Instant }
type PlayersData = Map Player MovingShapeTime

type GameState = {
  it :: Player
, itActive :: Boolean
, playersData :: PlayersData
}

initialGameState :: GameState
initialGameState =  
  { it: 0
  , itActive: false
  , playersData: Map.empty
  }

getCollision :: Player -> MovingShape -> PlayersData -> Maybe Player
getCollision player1 movingShape1 playersData =
  map getPlayer $ find isColliding $ 
    (Map.toUnfoldable $ Map.filterKeys (_ /= player1) playersData :: List _)
  where
  isColliding (Tuple _ {movingShape}) = shapesAreColliding movingShape1 movingShape
  getPlayer (Tuple player _) = player

data Action =
    HandleLobby Lobby.Output
  | ReceiveMessageFromPeer String
  | SetPlayer Player MovingShape
  | SetIt Player
  | HandleKeyDown H.SubscriptionId KeyboardEvent
  | HandleKeyUp H.SubscriptionId KeyboardEvent
  | Pulse

-- message types:
type PlayerMovingShape = { player :: Player, movingShape :: MovingShape }
type It = { it :: Player }

messageToAction :: String -> Either String Action
messageToAction msg = do
  json <- (parseJson msg) # (describeErr "Failed to parse message as JSON: ")
  (parseSetPlayer json <|||> parseSetIt json) # describeErrs "Failed to decode JSON:\n"
  where
  parseSetPlayer json = do
    ({player, movingShape} :: PlayerMovingShape) <- decodeJson json
    pure (SetPlayer player movingShape)
  parseSetIt json = do
    ({it} :: It) <- decodeJson json
    pure (SetIt it)

  describeErr :: forall b.String -> Either JsonDecodeError b -> Either String b
  describeErr s = mapLeft (\ err -> s <> (printJsonDecodeError err))
  describeErrs :: forall b.String -> Either (Array JsonDecodeError) b -> Either String b
  describeErrs s = mapLeft (\ errs -> s <> String.joinWith "\n" (map printJsonDecodeError errs))

_lobby :: SProxy "lobby"
_lobby = SProxy

_canvas :: SProxy "canvas"
_canvas = SProxy

type Slots = 
  ( lobby :: H.Slot Lobby.Query Lobby.Output Int
  , canvas :: H.Slot CanvasQuery Action Int )

data CanvasQuery a = CanvasQuery GameState a

passStateToCanvas :: forall action output m.
  H.HalogenM State action Slots output m Unit
passStateToCanvas = do
  {m_myPlayer, gameState} <- H.get
  case m_myPlayer of
    Nothing -> pure unit
    Just _ ->
      void $ H.query _canvas 1 $ H.tell (CanvasQuery gameState)

{-
  Adapted from https://gist.github.com/smilack/11c2fbb48fd85d811999880388e4fa9e
  "PureScript Halogen demo for drawing on a canvas using Hooks"
-}
canvasComponent ::
  forall input output m.
  MonadAff m =>
  Player -> H.Component HH.HTML CanvasQuery input output m
canvasComponent myPlayer =
  Hooks.component \{ queryToken } _ -> Hooks.do
    state /\ modifyState <- Hooks.useState initialGameState
    Hooks.useQuery queryToken case _ of
      CanvasQuery newState _ -> do
        Hooks.modify_ modifyState (const newState)
        pure Nothing
    drawOnCanvas state
    Hooks.pure $ 
      HH.canvas 
        [ HP.id_ "canvas"
        , HP.width (Int.ceil maxX)
        , HP.height (Int.ceil maxY) 
        ]
    where
    drawOnCanvas state =
      Hooks.do
        Hooks.captures {state} Hooks.useTickEffect do
          mcanvas <- liftEffect $ Canvas.getCanvasElementById "canvas"
          mcontext <- liftEffect $ sequence $ Canvas.getContext2D <$> mcanvas
          traverse_ drawPlayers mcontext
          pure Nothing
        Hooks.pure unit
      where
      drawPlayers context = liftEffect $ do
        drawBoard
        traverse_ drawPlayer (Map.toUnfoldable state.playersData :: List _)
        where
        drawBoard = do
          Canvas.setFillStyle context "lightgoldenrodyellow"
          Canvas.fillRect context { x: 0.0, y: 0.0, width: maxX, height: maxY }
        drawPlayer (Tuple player {movingShape: {center: {pos: {x,y}}, shape, radius}}) =  do
            Canvas.setFillStyle context playerStyle
            Canvas.fillPath context $ Canvas.arc context 
              { start: 0.0, end: 2.0*pi, radius, x, y }
            Canvas.setFillStyle context "black"
            Canvas.setFont context $ show textSize <> "px sans"
            Canvas.setTextAlign context AlignCenter
            -- Canvas.setTextBaseline context Canvas.BaselineTop -- not available in this version yet
            Canvas.fillText context shape x (y+0.6*radius)
            where
            playerStyle 
              | is_it = "lightcoral"
              | is_me = "bisque"
              | otherwise = "white"
            is_it = player == state.it
            is_me = player == myPlayer

            textSize = Int.round $ 1.6*radius
      
rootComponent :: forall input output query. H.Component HH.HTML query input output Aff
rootComponent =
  H.mkComponent
    { 
      initialState: \a -> initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      -- { handleAction = handleAction, initialize = Just Init }
      { handleAction = handleAction }
    }
  where
  render {m_myPlayer: Nothing} =
    HH.slot _lobby 0 (Lobby.component tigLobbySpec) unit (Just <<< HandleLobby)
  render {m_myPlayer: Just myPlayer, gameState: {it, playersData}} =
    HH.slot _canvas 1 (canvasComponent myPlayer) unit Just

  tigLobbySpec = sb do
    ae$
      { key: "shape"
      , maxLength: 5
      , description: "Player's name"
      , default: defaultShape
      }

  handleAction = case _ of
    -- messages from the lobby:
    HandleLobby (Connected ws) -> do
      H.modify_ $ \st -> st { m_ws = Just ws }
      -- start listening to the web socket:
      void $ H.subscribe $
        ES.EventSource.affEventSource \ emitter -> do
          fiber <- Aff.forkAff $ do
            setupWSListener ws (\msg -> ES.EventSource.emit emitter (ReceiveMessageFromPeer msg))
          pure $ ES.EventSource.Finalizer do
            Aff.killFiber (error "Event source finalized") fiber

      -- start pulse timer:
      void $ H.subscribe pulseTimer

    HandleLobby (SelectedPlayer player values) -> do
      -- set my player to the state:
      let shape = fromMaybe "?" $ Map.lookup "shape" values
      let movingShape = { center: initialMPt player, shape, radius: playerRadius }
      time <- liftEffect now
      H.modify_ $ 
        _ { m_myPlayer = Just player
          , gameState = 
            initialGameState { playersData = Map.singleton player {movingShape, time} } }
      -- force a Pulse now to sync with others asap:
      handleAction Pulse

      -- subscribe to keyboard events:
      document <- liftEffect $ Web.document =<< Web.window
      H.subscribe' \sid ->
        ES.eventListenerEventSource
          KET.keydown
          (HTMLDocument.toEventTarget document)
          (map (HandleKeyDown sid) <<< KE.fromEvent)
      H.subscribe' \sid ->
        ES.eventListenerEventSource
          KET.keyup
          (HTMLDocument.toEventTarget document)
          (map (HandleKeyUp sid) <<< KE.fromEvent)
      
    -- messages from peer players:
    ReceiveMessageFromPeer msg -> do
      case messageToAction msg of
        Left err -> liftEffect $ log err
        Right action -> handleAction action

    SetPlayer player movingShape -> do
      -- get current time:
      time <- liftEffect now

      -- update state:
      H.modify_ $ updateGameState $ \ st -> 
        st { playersData = Map.insert player {movingShape, time} st.playersData }
      passStateToCanvas      

      -- let the lobby know of this player:
      void $ H.query _lobby 0 $ H.tell (Lobby.NewPlayer player (Map.singleton "shape" movingShape.shape))

    SetIt it -> do
      H.modify_ $ updateGameState $ _ { it = it, itActive = false }
      -- passStateToCanvas

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
    Pulse -> do
      -- let others know we are still alive:
      handleMoveBy $ 
        MPt.move slowDownRatio
        >>> MPt.constrainSpeed maxSpeed 
        >>> MPt.constrainLocation maxX maxY
      
      -- find players who have not sent an update for some time:
      (Milliseconds timeNow) <- unInstant <$> liftEffect now
      let timeCutOff = timeNow - pulseTimeoutMs
      {m_myPlayer, gameState:{playersData,it}} <- H.get
      let deadPlayers = Map.filter (olderThan timeCutOff) playersData

      -- tell Lobby to remove these players:
      if Map.isEmpty deadPlayers then pure unit
        else do
          liftEffect $ log $ "deadPlayers = " <> show deadPlayers
          void $ H.query _lobby 0 $ 
            H.tell (Lobby.ClearPlayers $ Set.toUnfoldable $ Map.keys deadPlayers)

      -- delete the old players from state:
      let playersData2 = Map.filter (not <<< olderThan timeCutOff) playersData
      H.modify_ $ updateGameState $ _ { playersData = playersData2 }

      -- check whether "it" disappeared and if so, reassign it: 
      case m_myPlayer of
        Nothing -> pure unit
        Just myPlayer -> do
          -- check whether "it" exists
          let itGone = not $ Map.member it playersData2
          let m_minPlayer = Map.findMin playersData2
          case m_minPlayer of
            Just {key: minPlayer} | itGone && minPlayer == myPlayer -> do
              -- there is no "it" and we are the player with lowerst number, thus we should be it!
              H.modify_ $ updateGameState $ _ { it = myPlayer }
              {m_ws} <- H.get
              liftEffect $ sendIt m_ws myPlayer
            _ -> pure unit


  olderThan timeCutOff {time} =
    let (Milliseconds timeMs) = unInstant time in
    timeMs < timeCutOff

  handleMoveBy moveCenter = do
    {m_ws,m_myPlayer,gameState:{it,itActive,playersData}} <- H.get
    case m_myPlayer of
      Nothing -> pure unit
      Just myPlayer -> do
        case Map.lookup myPlayer playersData of
          Nothing -> pure unit
          Just {movingShape} -> do
            -- make the move locally:
            let newMovingShape = movingShape { center = moveCenter movingShape.center }
            time <- liftEffect now
            H.modify_ $ updateGameState $ \st -> 
              st { playersData = Map.insert myPlayer {movingShape: newMovingShape, time} st.playersData }
            passStateToCanvas
            -- send new position to peers:
            liftEffect $ sendMyPos m_ws {player: myPlayer, movingShape: newMovingShape}

            -- if I am it, check whether I caught someone:
            when (myPlayer == it && itActive) do
              case getCollision myPlayer newMovingShape playersData of
                Nothing -> pure unit
                Just player -> do
                    -- gotcha! update it locally:
                    H.modify_ $ updateGameState $ _ { it = player }
                    passStateToCanvas
                    -- and announce new "it":
                    liftEffect $ sendIt m_ws player
            -- if I am it but inactive, check whether I should become active:
            when (myPlayer == it && not itActive && (isNothing $ getCollision myPlayer newMovingShape playersData)) do
              -- not touching anyone any more, should become active now!
              H.modify_ $ updateGameState $ _ { itActive = true }
              passStateToCanvas


  sendMyPos (Just ws) playerPosShape = do
      WS.sendString ws $ stringify $ encodeJson playerPosShape
  sendMyPos _ _ = pure unit

  sendIt (Just ws) (it :: Player) = do
      WS.sendString ws $ stringify $ encodeJson {it}
  sendIt _ _ = pure unit

-- adapted from https://milesfrain.github.io/purescript-halogen/guide/04-Lifecycles-Subscriptions.html#implementing-a-timer
pulseTimer :: forall m. MonadAff m => ES.EventSource m Action
pulseTimer = ES.EventSource.affEventSource \emitter -> do
  fiber <- Aff.forkAff $ forever do
    Aff.delay $ Milliseconds pulsePeriodMs
    ES.EventSource.emit emitter Pulse

  pure $ ES.EventSource.Finalizer do
    Aff.killFiber (error "Event source finalized") fiber
