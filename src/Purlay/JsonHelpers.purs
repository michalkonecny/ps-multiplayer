{-|
    Module      :  Purlay.JsonHelpers
    Description :  Utilities for JSON serialisation
    Copyright   :  (c) Michal Konecny 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

   Utilities for JSON serialisation
-}
module Purlay.JsonHelpers where

import Prelude

import Data.Argonaut (class EncodeJson, Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class GDecodeJson)
import Data.Either (Either)
import Prim.RowList (class RowToList)

{-
  Existential type inspired by https://thimoteus.github.io/posts/2018-09-21-existential-types.html
-}
newtype AnyJsonable = AnyJsonable (forall r. (forall t. EncodeJson t => DecodeJsonWithSample t => t -> r) -> r)

anyJsonable :: forall t . EncodeJson t => DecodeJsonWithSample t => t -> AnyJsonable
anyJsonable v = AnyJsonable (_ $ v)

instance encodeJsonAnyJsonable :: EncodeJson AnyJsonable where
    encodeJson (AnyJsonable passValue) = passValue encodeJson

instance decodeJsonWithSampleAnyJsonable :: DecodeJsonWithSample AnyJsonable where
    decodeJsonWithSample (AnyJsonable passValue) json = 
        passValue (\v -> map anyJsonable (decodeJsonWithSample v json))

{-| A weaker version of DecodeJson needed for AnyJsonable -}
class DecodeJsonWithSample t where
    decodeJsonWithSample :: t -> Json -> Either JsonDecodeError t

instance decodeJsonWithSampleInt :: DecodeJsonWithSample Int where
    decodeJsonWithSample _ = decodeJson

instance decodeJsonWithSampleNumber :: DecodeJsonWithSample Number where
    decodeJsonWithSample _ = decodeJson

instance decodeJsonWithSampleString :: DecodeJsonWithSample String where
    decodeJsonWithSample _ = decodeJson

instance decodeJsonWithSampleRecord :: 
    (GDecodeJson row list, RowToList row list) 
    => 
    DecodeJsonWithSample (Record row) 
    where
    decodeJsonWithSample _ = decodeJson
