module Purlay.GameObjectRecord where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Either (Either(..))
import Purlay.MovingPoint (MovingPoint)
  
type GameObjectRecord a = 
  {
    shape :: Shape
  , consistency :: Consistency
  , scaling :: Number
  , xyState :: MovingPoint
  , angleState :: MovingAngle
  | a
  }

type MovingAngle = { angle :: Number, velo :: Number, accell :: Number }

initMovingAngle :: MovingAngle
initMovingAngle = { angle: 0.0, velo: 0.0, accell: 0.0 }

data Shape 
  = Ball { radius :: Number }
  -- | Rectangle { size :: XY }
  -- | Point -- ie no shape

derive instance eqShape :: Eq Shape

instance encodeJsonShape :: EncodeJson Shape where
  encodeJson (Ball { radius }) = encodeJson ({ shape: "Ball", radius })

instance decodeJsonShape :: DecodeJson Shape where
  decodeJson json = 
    case decodeJson json of 
      Right { shape: "Ball", radius } 
        -> Right $ Ball { radius }
      Right (_ :: { shape :: String, radius :: Number }) 
        -> Left $ UnexpectedValue json
      Left err 
        -> Left err

data Consistency = Solid -- | Container { openings :: Array XYbounds } | Phantom

derive instance eqConsistency :: Eq Consistency

instance encodeJsonConsistency :: EncodeJson Consistency where
  encodeJson Solid = encodeJson "Solid"

instance decodeJsonConsistency :: DecodeJson Consistency where
  decodeJson json = 
    case decodeJson json of 
      Right "Solid" -> Right Solid
      Right _       -> Left $ UnexpectedValue json
      Left err      -> Left err

