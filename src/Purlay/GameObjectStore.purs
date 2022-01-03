module Purlay.GameObjectStore
where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.TraversableWithIndex (traverseWithIndex)
import Purlay.GameObject (GameObject, unGO)
import Purlay.GameObject as GameObject
import Purlay.MovingShape as MShp

type GameObjectStore gstate objinfo action index = 
  Map.Map index (GameObject gstate objinfo action)

applyAction ::
  forall gstate objinfo action index m .
  Ord index =>
  Monad m =>
  { gstate :: gstate
  , action :: action
  , shouldApply :: index -> Boolean
  , store :: GameObjectStore gstate objinfo action index
  , processNewObject :: index -> GameObject gstate objinfo action -> m Unit
  } -> m (GameObjectStore gstate objinfo action index)
applyAction { gstate, action, shouldApply, store, processNewObject } = do
  traverseWithIndex maybeApply store
  where
  maybeApply index go
    | shouldApply index = 
      case (unGO go).applyAction gstate action of
        Nothing -> pure go
        Just newGO -> do
          processNewObject index newGO
          pure newGO
    | otherwise = pure go

applyCollisions ::
  forall gstate objinfo action index m .
  Ord index =>
  Monad m =>
  { shouldApply :: index -> Boolean
  , shouldApplyPair :: index -> index -> Boolean
  , store :: GameObjectStore gstate objinfo action index
  , processNewPair :: 
      index -> GameObject gstate objinfo action -> 
      index -> GameObject gstate objinfo action -> 
      m Unit
  } -> m (GameObjectStore gstate objinfo action index)
applyCollisions { shouldApply, shouldApplyPair, store, processNewPair } =
  foldlWithIndex maybeApply (pure store) store
  where
  maybeApply index1 prevStoreM go1
    | shouldApply index1 = do
        prevStore <- prevStoreM
        let {storeM} = foldlWithIndex (maybeApplyPair index1 go1) 
                        { foundCollision:false, storeM: pure prevStore } prevStore
        storeM
    | otherwise = prevStoreM
  maybeApplyPair index1 go1 index2 t@{foundCollision, storeM: store1M } go2
    | not foundCollision && shouldApplyPair index1 index2 = do
        case MShp.checkCollisionAndBounce { s1: (unGO go1).movingShape, s2: (unGO go2).movingShape } of
          Nothing -> t
          Just { new_s1, new_s2 } -> 
            { foundCollision: true, storeM: store2M }
              where
              new_go1 = GameObject.updateRec _{ movingShape = new_s1} go1
              new_go2 = GameObject.updateRec _{ movingShape = new_s2} go2
              store2M = do
                store1 <- store1M
                processNewPair index1 new_go1 index2 new_go2
                pure $ Map.insert index1 new_go1 $ Map.insert index2 new_go2 store1
    | otherwise = t

