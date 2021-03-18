module TickGame(mainTickGame) where

import Prelude

import Data.Argonaut (decodeJson, encodeJson, parseJson, printJsonDecodeError, stringify)
import Data.Array ((..))
import Data.Either (Either(..))
import Data.List.Lazy (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Effect.Exception (error)
import Halogen (ClassName(..), liftAff, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Halogen.Query.EventSource as ES.EventSource
import Halogen.VDom.Driver (runUI)
import WSListener (setupWSListener)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.Socket.ReadyState as ReadyState
import Web.Socket.WebSocket as WS
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

defaultWSURL :: String
defaultWSURL = "ws://localhost:3000"

playerShape :: String
playerShape = "😷"
-- playerShape = "X"

type Player = Int

playersForSelection :: Array Player
playersForSelection = [1,2,3]

maxX :: Int
maxX = 20
maxY :: Int
maxY = 20

type Pos = { x :: Int, y :: Int }
type PosShape = { pos :: Pos, shape :: String }

moveX :: Int -> PosShape -> PosShape
moveX dx {pos: {x,y}, shape}  = { pos: { x: ((x + (dx - 1)) `mod` maxX) + 1, y}, shape }

moveY :: Int -> PosShape -> PosShape
moveY dy {pos: {x,y}, shape} = { pos: { x, y: ((y + (dy - 1)) `mod` maxY) + 1}, shape }


data State = Connect | Entry EntryState | Play PlayState

updatePlay :: (PlayState -> PlayState) -> State -> State
updatePlay fn = case _ of
  Play playState -> Play $ fn playState
  state -> state

updateEntry :: (EntryState -> EntryState) -> State -> State
updateEntry fn = case _ of
  Entry entryState -> Entry $ fn entryState
  state -> state

type EntryState = {  
  ws :: WS.WebSocket
, it :: Player
, blobs :: Map Player PosShape
}

type PlayState = {
  ws :: WS.WebSocket
, myPlayer :: Player
, it :: Player
, blobs :: Map Player PosShape
}

initialState :: State
initialState = Connect

initialPosShape :: Player -> PosShape
initialPosShape player = 
  { pos: { x: 1 + player*5 `mod` maxX, y: 1 + player*5 `mod` maxY }, shape: playerShape }

byPos :: Map Player PosShape -> Map Pos { player :: Player, shape :: String }
byPos blobs = 
  Map.fromFoldable $ 
  map (\(Tuple player {pos, shape}) -> Tuple pos {player, shape}) $ 
  (Map.toUnfoldable blobs :: List _)

data Action =
    SetWSURL String
  | SetMyPlayer Player
  | ReceiveMessageFromPeer String
  | SetPlayer Player PosShape
  | SetIt Player
  | HandleKey H.SubscriptionId KeyboardEvent

type PlayerPosShape = { player :: Player, posShape :: PosShape }
type It = { it :: Player }

rootComponent :: forall input output query. H.Component HH.HTML query input output Aff
rootComponent =
  H.mkComponent
    { 
      initialState: \a -> initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      -- { handleAction = handleAction, initialize = Just Init }
      { handleAction = handleAction
      , initialize = Just (SetWSURL defaultWSURL) }
    }
  where
  render Connect =
    HH.div [] [ enterWSURL ]
    where
    enterWSURL = 
      HH.div_ [HH.span_ 
        [HH.text "Enter web-socket URL: ws://", 
         HH.input [HP.value (String.drop 5 defaultWSURL), HE.onValueChange (Just <<< SetWSURL <<< ("ws://" <> _)) ]]]
  render (Entry {it, blobs}) =
    HH.div [] [ choosePlayer ]
    where
    choosePlayer = HH.table_ $ [HH.tr_ $ map playerButton playersForSelection]
    playerButton player = HH.td_ $ [HH.button [HP.title label, HE.onClick \_ -> Just (SetMyPlayer player)] [ HH.text label ]]
      where
      label = "Player " <> show player
  render (Play {myPlayer, it, blobs}) =
    HH.div [] [ gameBoard ]
    where
    gameBoard = 
      HH.table [HP.class_ (ClassName "gameBoard")] rows
      where
      rows = map makeRow (1..maxY)
      makeRow y = HH.tr_ $ map (makeCell y) (1..maxX)
      makeCell y x = HH.td [HP.class_ (ClassName cellClass)] [HH.text cellText]
        where
        posXY = {x,y}
        Tuple cellText cellClass = 
          case posXY `Map.lookup` byPos blobs of
            Just {player, shape} 
              | player == it && it == myPlayer -> Tuple shape "itMyPlayerCell"
              | player == it -> Tuple shape "itPlayerCell"
              | player == myPlayer -> Tuple shape "myPlayerCell"
              | otherwise -> Tuple shape "playerCell"
            _ -> Tuple "" "blankCell"


  handleAction = case _ of
    SetWSURL wsURL -> do
      ws <- liftEffect $ WS.create wsURL []
      liftAff $ delay (Milliseconds 100.0) -- allow ws to initialise
      rs <- liftEffect $ WS.readyState ws
      if rs /= ReadyState.Open
        then do
          liftEffect $ log $ "failed to open web-socket at " <> wsURL
        else do
          H.put $ Entry { ws, it: 1, blobs: Map.empty }

          -- start listening to the web socket:
          void $ H.subscribe $
            ES.EventSource.affEventSource \ emitter -> do
              fiber <- Aff.forkAff $ do
                setupWSListener ws (\msg -> ES.EventSource.emit emitter (ReceiveMessageFromPeer msg))
              pure $ ES.EventSource.Finalizer do
                Aff.killFiber (error "Event source finalized") fiber

    SetMyPlayer player -> do
      st <- H.get
      case st of
        Entry {ws, it, blobs} -> do
          -- set the this player to the state:
          let posShape = initialPosShape player
          H.put $ Play { ws, myPlayer: player, it, blobs: Map.insert player posShape blobs }
          -- let others know our position:
          liftEffect $ sendMyPos ws {player, posShape}
          -- start listening to keyboard events:
        _ -> do
          liftEffect $ log $ "MyPlayer called at an illegal state"

      -- subscribe to keyboard events:
      document <- liftEffect $ Web.document =<< Web.window
      H.subscribe' \sid ->
        ES.eventListenerEventSource
          KET.keydown
          (HTMLDocument.toEventTarget document)
          (map (HandleKey sid) <<< KE.fromEvent)
      
    ReceiveMessageFromPeer msg -> do
      case parseJson msg of
        Left err -> liftEffect $ log  $ "Failed to parse message as JSON: " <> (printJsonDecodeError err)
        Right json ->
          case decodeJson json of
            Right ({player, posShape} :: PlayerPosShape) ->
              handleAction (SetPlayer player posShape)
            Left err1 -> 
              case decodeJson json of
                Right ({it} :: It) ->
                  handleAction (SetIt it)
                Left err2 -> 
                  liftEffect $ log  $ 
                    "Failed to decode JSON:\n" <> (printJsonDecodeError err1) 
                    <> "\n" <> (printJsonDecodeError err2)

    SetPlayer player posShape -> do
      -- if this is a new player, send out my position for them to see:
      state <- H.get
      case state of
        Connect -> 
          liftEffect $ log $ "SetPlayer called before connection established..."
        Entry {it,blobs} ->
          H.modify_ $ updateEntry $ \st -> st {blobs = Map.insert player posShape st.blobs}
        Play (playState@{ws,myPlayer,it,blobs}) -> do
          case player `Map.member` blobs of
            true -> pure unit
            false -> do
              handleMoveBy identity -- ie do not move, but still send out our position
              liftEffect $ sendIt ws it -- and send them also who is "it"
          -- update state:
          H.put $ Play $ playState { blobs = Map.insert player posShape blobs }
          -- if I am it, check whether I caught them:
          let {pos} = posShape
          case Map.lookup myPlayer blobs of
            Nothing -> pure unit
            Just {pos: myPos} -> do
              if myPlayer == it && pos == myPos
                then do
                  -- gotcha! update it locally:
                  H.modify_ $ updatePlay $ \st -> st { it = player }
                  -- and announce new "it":
                  liftEffect $ sendIt ws player
                else pure unit
    SetIt it -> do
      H.modify_ $ 
        (updateEntry $ \st -> st { it = it }) <<<
        (updatePlay $ \st -> st { it = it })
    HandleKey _sid ev -> do
      case KE.key ev of
        "ArrowLeft" -> handleMoveBy (moveX (-1))
        "ArrowRight" -> handleMoveBy (moveX (1))
        "ArrowUp" -> handleMoveBy (moveY (-1))
        "ArrowDown" -> handleMoveBy (moveY (1))
        _ -> pure unit

  handleMoveBy fn = do
    state <- H.get
    case state of
      Play {ws,myPlayer,it,blobs} ->
        case Map.lookup myPlayer blobs of
          Nothing -> pure unit
          Just posShape -> do
            -- make the move locally:
            let newPosShape = fn posShape
            H.modify_ $ updatePlay $ \st -> st { blobs = Map.insert myPlayer newPosShape st.blobs }
            -- send new position to peers:
            liftEffect $ sendMyPos ws {player: myPlayer, posShape: newPosShape}
            -- if I am it, check whether I caught someone:
            if myPlayer == it
              then do
                let {pos} = newPosShape
                case Map.lookup pos (byPos blobs) of
                  Just {player} | player /= myPlayer -> do
                    -- gotcha! update it locally:
                    H.modify_ $ updatePlay \st -> st { it = player }
                    -- and announce new "it":
                    liftEffect $ sendIt ws player
                  _ -> pure unit
              else pure unit
      _ -> pure unit    

  sendMyPos ws playerPosShape = do
      WS.sendString ws $ stringify $ encodeJson playerPosShape

  sendIt ws (it :: Player) = do
      WS.sendString ws $ stringify $ encodeJson {it}

-- MAIN

mainTickGame :: Effect Unit
mainTickGame = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI rootComponent unit body
    
