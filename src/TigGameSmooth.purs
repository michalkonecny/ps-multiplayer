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
import EitherHelpers (mapLeft, (<|||>))
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
import MovingBall (MovingBall, ballBounceOffBall, drawBall)
import MovingPoint (MovingPoint, constrainLocation)
import MovingPoint as MPt
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

type Name = String

defaultName :: Name
defaultName = "????"

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
  m_ws :: Maybe WebSocket
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

type MovingBallTime = { movingBall :: MovingBall, time :: Instant }
type PlayersData = Map Player MovingBallTime

type GameState = {
  it :: Player
, itActive :: Boolean
, playersData :: PlayersData
}

initialGameState :: GameState
initialGameState =  
  { it: 0
  , itActive: true
  , playersData: Map.empty
  }

getCollision :: Player -> MovingBall -> PlayersData -> Maybe (Tuple Player MovingBall)
getCollision player1 movingBall1 playersData =
  removeJust $ find (\(Tuple _ m) -> isJust m) $ 
    (Map.toUnfoldable $ map (\{movingBall} -> ballBounceOffBall movingBall movingBall1) $ 
      Map.filterKeys (_ /= player1) playersData :: List _)
  where
  removeJust (Just (Tuple p (Just mb))) = Just (Tuple p mb)
  removeJust _ = Nothing

data Action =
    HandleLobby Lobby.Output
  | ReceiveMessageFromPeer String
  | SetPlayer Player MovingBall
  | SetIt Player
  | HandleKeyDown H.SubscriptionId KeyboardEvent
  | HandleKeyUp H.SubscriptionId KeyboardEvent
  | Pulse

-- message types:
type MovingPlayer = { player :: Player, movingBall :: MovingBall }
type It = { it :: Player }

sendMyPos :: Maybe WebSocket -> MovingPlayer -> Effect Unit
sendMyPos (Just ws) playerPosName = do
    WS.sendString ws $ stringify $ encodeJson playerPosName
sendMyPos _ _ = pure unit

sendIt :: Maybe WebSocket -> Player -> Effect Unit
sendIt (Just ws) (it :: Player) = do
    WS.sendString ws $ stringify $ encodeJson {it}
sendIt _ _ = pure unit

messageToAction :: String -> Either String Action
messageToAction msg = do
  json <- (parseJson msg) # (describeErr "Failed to parse message as JSON: ")
  (parseSetPlayer json <|||> parseSetIt json) # describeErrs "Failed to decode JSON:\n"
  where
  parseSetPlayer json = do
    ({player, movingBall} :: MovingPlayer) <- decodeJson json
    pure (SetPlayer player movingBall)
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

passStateToCanvas :: forall output.
  H.HalogenM State Action Slots output Aff Unit
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
        drawPlayer (Tuple player {movingBall}) = 
            drawBall context playerStyle movingBall
            where
            playerStyle 
              | is_it = "lightcoral"
              | is_me = "bisque"
              | otherwise = "white"
            is_it = player == state.it
            is_me = player == myPlayer
      
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
      { key: "name"
      , maxLength: 5
      , description: "Player's name"
      , default: defaultName
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
      let name = fromMaybe "?" $ Map.lookup "name" values
      let movingBall = { center: initialMPt player, name, radius: playerRadius }
      time <- liftEffect now
      H.modify_ $ 
        _ { m_myPlayer = Just player
          , gameState = 
            initialGameState { playersData = Map.singleton player {movingBall, time} } }
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

    SetPlayer player movingBall -> do
      -- get current time:
      time <- liftEffect now

      -- update state:
      H.modify_ $ updateGameState $ \ st -> 
        st { playersData = Map.insert player {movingBall, time} st.playersData }
      passStateToCanvas      

      -- let the lobby know of this player:
      void $ H.query _lobby 0 $ H.tell (Lobby.NewPlayer player (Map.singleton "name" movingBall.name))

    SetIt it -> do
      H.modify_ $ updateGameState $ _ { it = it, itActive = false }
      {m_myPlayer} <- H.get
      pure unit
      case m_myPlayer of
        Just myPlayer | it == myPlayer -> do
          liftAff $ delay (Milliseconds 1000.0) -- 1 second
          H.modify_ $ updateGameState $ _ { itActive = true }
        _ -> pure unit
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
      -- find players who have not sent an update for some time:
      (Milliseconds timeNow) <- unInstant <$> liftEffect now
      let timeCutOff = timeNow - pulseTimeoutMs
      {m_myPlayer, gameState:{playersData,it}} <- H.get
      let deadPlayers = Map.filter (olderThan timeCutOff) playersData

      -- tell Lobby to remove these players:
      if Map.isEmpty deadPlayers then pure unit
        else do
          -- liftEffect $ log $ "deadPlayers = " <> show deadPlayers
          void $ H.query _lobby 0 $ 
            H.tell (Lobby.ClearPlayers $ Set.toUnfoldable $ Map.keys deadPlayers)

      -- delete the old players from state:
      let playersData2 = Map.filter (not <<< olderThan timeCutOff) playersData
      H.modify_ $ updateGameState $ _ { playersData = playersData2 }

      -- are we in the game play stage?
      case m_myPlayer of
        Nothing -> pure unit
        Just myPlayer -> do
          -- let others know we are still alive:
          handleMoveBy $ 
            MPt.move slowDownRatio
            >>> MPt.constrainSpeed maxSpeed 
            >>> MPt.constrainLocation maxX maxY
          
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
          Just {movingBall} -> do
            -- make the move locally:
            let newMovingBall = movingBall { center = moveCenter movingBall.center }
            time <- liftEffect now
            H.modify_ $ updateGameState $ \st -> 
              st { playersData = Map.insert myPlayer {movingBall: newMovingBall, time} st.playersData }
            passStateToCanvas

            -- send new position to peers:
            liftEffect $ sendMyPos m_ws {player: myPlayer, movingBall: newMovingBall}

            -- check for a collision:
            case getCollision myPlayer newMovingBall playersData of
              Nothing -> pure unit
              Just (Tuple player newMovingBall2) -> do -- collision occurred, bounced off another player
                  -- change my movement due to the bounce:
                  H.modify_ $ updateGameState $ \st -> 
                    st { playersData = Map.insert myPlayer {movingBall: newMovingBall2, time} st.playersData }
                  passStateToCanvas

                -- if I am it, tig them!
                  when (myPlayer == it && itActive) do
                    -- gotcha! update it locally:
                    H.modify_ $ updateGameState $ _ { it = player }
                    passStateToCanvas
                    -- and announce new "it":
                    liftEffect $ sendIt m_ws player

-- adapted from https://milesfrain.github.io/purescript-halogen/guide/04-Lifecycles-Subscriptions.html#implementing-a-timer
pulseTimer :: forall m. MonadAff m => ES.EventSource m Action
pulseTimer = ES.EventSource.affEventSource \emitter -> do
  fiber <- Aff.forkAff $ forever do
    Aff.delay $ Milliseconds pulsePeriodMs
    ES.EventSource.emit emitter Pulse

  pure $ ES.EventSource.Finalizer do
    Aff.killFiber (error "Event source finalized") fiber
