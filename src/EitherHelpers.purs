{-|
    Module      :  EitherHelpers
    Description :  Utilities for Either values
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

   Utilities for Either values
-}
module EitherHelpers where

import Prelude

import Data.Either (Either(..))

mapLeft :: forall a a' b. (a -> a') -> Either a b -> Either a' b
mapLeft fn (Left a) = Left (fn a)
mapLeft _ (Right b) = Right b

altMergeLefts :: forall a b. Either a b -> Either a b -> Either (Array a) b
altMergeLefts (Right b) _ = Right b
altMergeLefts (Left _) (Right b) = Right b
altMergeLefts (Left a1) (Left a2) = Left [a1,a2]

infixl 3 altMergeLefts as <|||>

altMergeLeftsAdd :: forall a b. Either (Array a) b -> Either a b -> Either (Array a) b
altMergeLeftsAdd (Right b) _ = Right b
altMergeLeftsAdd (Left _) (Right b) = Right b
altMergeLeftsAdd (Left as) (Left a) = Left (as <> [a])

infixl 3 altMergeLeftsAdd as <||>

