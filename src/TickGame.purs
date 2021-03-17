module TickGame(mainTickGame) where

import Prelude

import Data.Argonaut (decodeJson, encodeJson, parseJson, printJsonDecodeError, stringify)
import Data.Array ((..))
import Data.Either (Either(..))
import Data.List.Lazy (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class.Console (log)
import Halogen (ClassName(..), liftAff, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Halogen.VDom.Driver (runUI)
import HalogenWS (RootQuery(..), setupWSListener)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.Socket.WebSocket as WS
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

wsURL :: String
wsURL = "ws://192.168.1.197:3000"
-- wsURL = "ws://localhost:3000"

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

type State = {
  maybeMyPlayer :: Maybe Player
, it :: Player
, blobs :: Map Player PosShape
}

initialState :: State
initialState = 
  {
    maybeMyPlayer: Nothing
  , it: 1
  , blobs: Map.empty
  }

initialPosShape :: Player -> PosShape
initialPosShape player = 
  { pos: { x: 1 + player*5 `mod` maxX, y: 1 + player*5 `mod` maxY }, shape: playerShape }

data Action =
    MyPlayer Player
  | HandleKey H.SubscriptionId KeyboardEvent
  | SetPlayer Player PosShape
  | SetIt Player

type PlayerPosShape = { player :: Player, posShape :: PosShape }
type It = { it :: Player }

rootComponent :: forall input output. WS.WebSocket -> H.Component HH.HTML RootQuery input output Aff
rootComponent ws =
  H.mkComponent
    { 
      initialState: \a -> initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      -- { handleAction = handleAction, initialize = Just Init }
      { handleAction = handleAction,
        handleQuery = handleQuery }
    }
  where
  render {maybeMyPlayer, it, blobs} =
    case maybeMyPlayer of
      Nothing -> HH.div [] [ choosePlayer ]
      Just myPlayer -> HH.div [] [ gameBoard myPlayer ]
    where
    choosePlayer = HH.table_ $ [HH.tr_ $ map playerButton playersForSelection]
    playerButton player = HH.td_ $ [HH.button [HP.title label, HE.onClick \_ -> Just (MyPlayer player)] [ HH.text label ]]
      where
      label = "Player " <> show player
    gameBoard myPlayer = 
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

  byPos blobs = 
    Map.fromFoldable $ 
    map (\(Tuple player {pos, shape}) -> Tuple pos {player, shape}) $ 
    (Map.toUnfoldable blobs :: List _)

  handleQuery :: forall a. RootQuery a -> H.HalogenM State Action () output Aff (Maybe a)
  handleQuery = case _ of
    ReceiveMessage msg a -> do
      -- liftEffect $ log $ "NewMessage " <> msg
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
      pure (Just a)

  handleAction = case _ of
    MyPlayer player -> do
      -- set the this player to the state:
      let posShape = initialPosShape player
      H.modify_ \st -> st { maybeMyPlayer = Just player, blobs = Map.insert player posShape st.blobs }
      -- let others know our position:
      liftEffect $ sendMyPos {player, posShape}

      -- subscribe to keyboard events:
      document <- liftEffect $ Web.document =<< Web.window
      H.subscribe' \sid ->
        ES.eventListenerEventSource
          KET.keydown
          (HTMLDocument.toEventTarget document)
          (map (HandleKey sid) <<< KE.fromEvent)
      
    HandleKey _sid ev -> do
      case KE.key ev of
        "ArrowLeft" -> handleMoveBy (moveX (-1))
        "ArrowRight" -> handleMoveBy (moveX (1))
        "ArrowUp" -> handleMoveBy (moveY (-1))
        "ArrowDown" -> handleMoveBy (moveY (1))
        _ -> pure unit
    SetPlayer player posShape -> do
      -- if this is a new player, send out my position for them to see:
      {blobs,it} <- H.get
      maybePlayerPosShape <- getMyPosShape <$> H.get
      case player `Map.member` blobs of
        true -> pure unit
        false -> do
          handleMoveBy identity -- ie do not move, but still send out our position
          liftEffect $ sendIt it -- and send them also who is "it"
      -- update state:
      H.modify_ \st -> st { blobs = Map.insert player posShape st.blobs }
      -- if I am it, check whether I caught them:
      let {pos} = posShape
      case maybePlayerPosShape of
        Nothing -> pure unit
        Just {player: myPlayer, posShape: {pos: myPos}} -> do
          if myPlayer == it && pos == myPos
            then do
              -- gotcha! update it locally:
              H.modify_ \st -> st { it = player }
              -- and announce new "it":
              liftEffect $ sendIt player
            else pure unit
    SetIt it -> do
      H.modify_ \st -> st { it = it }

  handleMoveBy fn = do
    maybePlayerPosShape <- getMyPosShape <$> H.get
    {it,blobs} <- H.get
    case maybePlayerPosShape of
      Nothing -> pure unit
      Just {player: myPlayer, posShape} -> do
        -- make the move locally:
        let newPosShape = fn posShape
        H.modify_ \st -> st { blobs = Map.insert myPlayer newPosShape st.blobs }
        -- send new position to peers:
        liftEffect $ sendMyPos {player: myPlayer, posShape: newPosShape}
        -- if I am it, check whether I caught someone:
        if myPlayer == it
          then do
            let {pos} = newPosShape
            case Map.lookup pos (byPos blobs) of
              Just {player} | player /= myPlayer -> do
                -- gotcha! update it locally:
                H.modify_ \st -> st { it = player }
                -- and announce new "it":
                liftEffect $ sendIt player
              _ -> pure unit
          else pure unit

  getMyPosShape :: State -> Maybe PlayerPosShape
  getMyPosShape {maybeMyPlayer,blobs} =
    case maybeMyPlayer of
      Nothing -> Nothing
      Just player ->
        case player `Map.lookup` blobs of
          Nothing -> Nothing
          Just posShape -> Just {player, posShape}

  sendMyPos playerPosShape = do
      WS.sendString ws $ stringify $ encodeJson playerPosShape

  sendIt (it :: Player) = do
      WS.sendString ws $ stringify $ encodeJson {it}

-- MAIN

mainTickGame :: Effect Unit
mainTickGame = do
  ws <- WS.create wsURL []
  HA.runHalogenAff do
    liftAff $ delay (Milliseconds 100.0) -- allow ws to initialise
    body <- HA.awaitBody
    io <- runUI (rootComponent ws) unit body
    setupWSListener ws io
