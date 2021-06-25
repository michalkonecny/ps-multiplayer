{-|
    Module      :  Purlay.Examples.TigGame.GState
    Description :  A player's piece
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Purlay.Examples.TigGame.GState where

-- import Prelude

import Purlay.Coordinator (PeerId)

newtype GState = GState GState_record

type GState_record = {
  it :: PeerId
}

initGState :: GState
initGState = GState {
  it: 0
}