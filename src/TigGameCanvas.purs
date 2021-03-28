{-|
    Module      :  TigGameCanvas
    Description :  A simple multiplayer game of tig
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

   A simple multiplayer game of tig
-}
module TigGameCanvas(mainTigGameCanvas) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Argonaut (JsonDecodeError, decodeJson, encodeJson, parseJson, printJsonDecodeError, stringify)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.List.Lazy (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, sequence_, traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Effect.Now (now)
import EitherHelpers (mapLeft, (<|||>))
import Graphics.Canvas as Canvas
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (Hook)
import Halogen.Hooks as Hooks
import Halogen.Query.EventSource as ES
import Halogen.Query.EventSource as ES.EventSource
import Halogen.VDom.Driver (runUI)
import Lobby (Output(..), Player, Shape)
import Lobby as Lobby
import WSListener (setupWSListener)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.Socket.WebSocket as WS
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

mainTigGameCanvas :: Effect Unit
mainTigGameCanvas = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI rootComponent unit body

pulsePeriodMs :: Number
pulsePeriodMs = 500.0

pulseTimeoutMs :: Number
pulseTimeoutMs = 2000.0

maxX :: Int
maxX = 20
maxY :: Int
maxY = 20

type Pos = { x :: Int, y :: Int }
type PosShape = { pos :: Pos, shape :: Shape }
type PosShapeTime = { posShape :: PosShape, time :: Instant }

initialPos :: Player -> Pos
initialPos player = 
  { x: 1 + (player*3) `mod` maxX, y: 1 + (player*3) `mod` maxY }

moveX :: Int -> PosShape -> PosShape
moveX dx {pos: {x,y}, shape}  = { pos: { x: ((x + (dx - 1)) `mod` maxX) + 1, y}, shape }

moveY :: Int -> PosShape -> PosShape
moveY dy {pos: {x,y}, shape} = { pos: { x, y: ((y + (dy - 1)) `mod` maxY) + 1}, shape }

type State = {
  m_ws :: Maybe WS.WebSocket
, m_GameState :: Maybe GameState
}

updateGameState :: (GameState -> GameState) -> State -> State
updateGameState fn state = case state of
  {m_GameState: Just gameState} -> state {m_GameState = Just $ fn gameState}
  _ -> state

initialState :: State
initialState =  
  { m_ws: Nothing 
  , m_GameState: Nothing
  }

type GameState = {
  myPlayer :: Player
, it :: Player
, playersData :: Map Player PosShapeTime
}

initialGameState :: GameState
initialGameState =  
  { myPlayer: 0
  , it: 1
  , playersData: Map.empty
  }

byPos :: Map Player PosShapeTime -> Map Pos { player :: Player, shape :: String }
byPos playersData = 
  Map.fromFoldable $ 
  map (\(Tuple player {posShape: {pos, shape}}) -> Tuple pos {player, shape}) $ 
  (Map.toUnfoldable playersData :: List _)

data Action =
    HandleLobby Lobby.Output
  | ReceiveMessageFromPeer String
  | SetPlayer Player PosShape
  | SetIt Player
  | HandleKey H.SubscriptionId KeyboardEvent
  | Pulse

-- message types:
type PlayerPosShape = { player :: Player, posShape :: PosShape }
type It = { it :: Player }

messageToAction :: String -> Either String Action
messageToAction msg = do
  json <- (parseJson msg) # (describeErr "Failed to parse message as JSON: ")
  (parseSetPlayer json <|||> parseSetIt json) # describeErrs "Failed to decode JSON:\n"
  where
  parseSetPlayer json = do
    ({player, posShape} :: PlayerPosShape) <- decodeJson json
    pure (SetPlayer player posShape)
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
  {m_GameState} <- H.get
  case m_GameState of
    Nothing -> pure unit
    Just st ->
      void $ H.query _canvas 1 $ H.tell (CanvasQuery st)

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
    Hooks.pure $ HH.canvas [ HP.id_ "canvas", HP.width (40*maxX), HP.height (40*maxY) ]

newtype DrawOnCanvas hooks
  = DrawOnCanvas (Hooks.UseEffect hooks)

derive instance newtypeDrawOnCanvas :: Newtype (DrawOnCanvas hooks) _

drawOnCanvas :: forall m. MonadAff m => GameState -> Hook m DrawOnCanvas Unit
drawOnCanvas state =
  Hooks.wrap Hooks.do
    Hooks.captures {state} Hooks.useTickEffect do
      mcanvas <- liftEffect $ Canvas.getCanvasElementById "canvas"
      mcontext <- liftEffect $ sequence $ Canvas.getContext2D <$> mcanvas
      traverse_ drawPlayers mcontext
      pure Nothing
    Hooks.pure unit
  where
  drawPlayers context = liftEffect $ do
    Canvas.setFillStyle context "lightgreen"
    Canvas.fillRect context { x: 0.0, y: 0.0, width: (toNumber maxX) * 40.0, height: (toNumber maxY) * 40.0 }
    sequence_ $ map drawPlayer $ (Map.toUnfoldable state.playersData :: List _)
    where
    drawPlayer (Tuple player {posShape: {pos: {x: px, y: py}, shape}}) =  do
      Canvas.setFillStyle context "red"
      Canvas.fillRect context { x, y, width: 40.0, height: 40.0 }
      where
      x = 40.0 * (toNumber px - 1.0)
      y = 40.0 * (toNumber py - 1.0)
      

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
  render {m_GameState: Nothing} =
    HH.slot _lobby 0 Lobby.component unit (Just <<< HandleLobby)
  render {m_GameState: Just { myPlayer, it, playersData}} =
    HH.slot _canvas 1 (canvasComponent myPlayer) unit Just

  handleAction = case _ of
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

    HandleLobby (SelectedPlayer player shape) -> do
      -- set my player to the state:
      let posShape = { pos: initialPos player, shape }
      time <- liftEffect now
      H.modify_ $ _ { m_GameState = Just $
          initialGameState 
              { myPlayer = player
              , playersData = Map.singleton player {posShape, time} } }
      -- let others know our position:
      {m_ws} <- H.get
      liftEffect $ sendMyPos m_ws {player, posShape}

      -- subscribe to keyboard events:
      document <- liftEffect $ Web.document =<< Web.window
      H.subscribe' \sid ->
        ES.eventListenerEventSource
          KET.keydown
          (HTMLDocument.toEventTarget document)
          (map (HandleKey sid) <<< KE.fromEvent)
      
    ReceiveMessageFromPeer msg -> do
      case messageToAction msg of
        Left err -> liftEffect $ log err
        Right action -> handleAction action

    SetPlayer player posShape -> do
      -- get current time:
      time <- liftEffect now

      -- let the lobby know of this player:
      void $ H.query _lobby 0 $ H.tell (Lobby.NewPlayer player)

      {m_ws,m_GameState} <- H.get

      case m_GameState of
        Nothing -> pure unit
        Just {myPlayer,it,playersData} -> do
          -- if this is a new player, send out my position for them to see:
          case player `Map.member` playersData of
            true -> pure unit
            false -> do
              handleMoveBy identity -- ie do not move, but still send out our position
              liftEffect $ sendIt m_ws it -- and send them also who is "it"
          -- update state:
          H.modify_ $ updateGameState $ \ st -> 
            st { playersData = Map.insert player {posShape, time} st.playersData }
          passStateToCanvas

          -- if I am it, check whether I caught them:
          let {pos} = posShape
          case Map.lookup myPlayer playersData of
            Nothing -> pure unit
            Just {posShape: {pos: myPos}} -> do
              if myPlayer == it && pos == myPos
                then do
                  -- gotcha! update it locally:
                  H.modify_ $ updateGameState $ _ { it = player }
                  passStateToCanvas
                  -- and announce new "it":
                  liftEffect $ sendIt m_ws player
                else pure unit
    SetIt it -> do
      H.modify_ $ updateGameState $ _ { it = it }
      passStateToCanvas
    HandleKey _sid ev -> do
      case KE.key ev of
        "ArrowLeft" -> handleMoveBy (moveX (-1))
        "ArrowRight" -> handleMoveBy (moveX (1))
        "ArrowUp" -> handleMoveBy (moveY (-1))
        "ArrowDown" -> handleMoveBy (moveY (1))
        _ -> pure unit
    Pulse -> do
      -- let others know we are still alive:
      handleMoveBy identity
      
      -- find players who have not sent an update for some time:
      (Milliseconds timeNow) <- unInstant <$> liftEffect now
      let timeCutOff = timeNow - pulseTimeoutMs
      {m_GameState} <- H.get
      case m_GameState of
        Nothing -> pure unit
        Just {playersData} -> do
          let deadPlayers = Map.filter (olderThan timeCutOff) playersData

          -- tell Lobby to remove these players:
          if Map.isEmpty deadPlayers then pure unit
            else void $ H.query _lobby 0 $ H.tell (Lobby.ClearPlayers $ Map.keys deadPlayers)

          -- delete the old players from state:
          H.modify_ $ updateGameState $ \st -> 
            st { playersData = Map.filter (not <<< olderThan timeCutOff) st.playersData }

  olderThan timeCutOff {time} =
    let (Milliseconds timeMs) = unInstant time in
    timeMs < timeCutOff
  handleMoveBy fn = do
    {m_ws,m_GameState} <- H.get
    case m_GameState of
      Nothing -> pure unit
      Just {myPlayer,it,playersData} -> do
        case Map.lookup myPlayer playersData of
          Nothing -> pure unit
          Just {posShape} -> do
            -- make the move locally:
            let newPosShape = fn posShape
            time <- liftEffect now
            H.modify_ $ updateGameState $ \st -> 
              st { playersData = Map.insert myPlayer {posShape: newPosShape, time} st.playersData }
            passStateToCanvas
            -- send new position to peers:
            liftEffect $ sendMyPos m_ws {player: myPlayer, posShape: newPosShape}
            -- if I am it, check whether I caught someone:
            if myPlayer == it
              then do
                let {pos} = newPosShape
                case Map.lookup pos (byPos playersData) of
                  Just {player} | player /= myPlayer -> do
                    -- gotcha! update it locally:
                    H.modify_ $ updateGameState $ _ { it = player }
                    passStateToCanvas
                    -- and announce new "it":
                    liftEffect $ sendIt m_ws player
                  _ -> pure unit
              else pure unit

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

lookupMaybe :: forall k v . (Ord k) => Maybe k -> Map k v -> Maybe (Tuple k v)
lookupMaybe (Just a) map = 
  case Map.lookup a map of
    Just b -> Just (Tuple a b)
    _ -> Nothing
lookupMaybe _ _ = Nothing
