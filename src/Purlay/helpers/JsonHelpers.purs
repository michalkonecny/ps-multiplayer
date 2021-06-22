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

{-|
  A union of all Jsonable types

  Existential type inspired by https://thimoteus.github.io/posts/2018-09-21-existential-types.html
-}
newtype AnyJsonable = AnyJsonable (forall r. (forall t. Jsonable t => t -> r) -> r)

anyJsonable :: forall t . Jsonable t => t -> AnyJsonable
anyJsonable v = AnyJsonable (_ $ v)

instance encodeJsonAnyJsonable :: EncodeJson AnyJsonable where
    encodeJson (AnyJsonable passValue) = passValue encodeJson

instance decodeJsonWithSampleAnyJsonable :: DecodeJsonWithSample AnyJsonable where
    decodeJsonWithSample (AnyJsonable passValue) json = 
        passValue (\v -> map anyJsonable (decodeJsonWithSample v json))

{-| A conjunction of EncodeJson and a variant of DecodeJson -}
class (EncodeJson t, DecodeJsonWithSample t) <= Jsonable t

instance jsonableInt :: Jsonable Int
instance jsonableNumber :: Jsonable Number
instance jsonableString :: Jsonable String
instance jsonableRecord :: 
    (GDecodeJson row list, RowToList row list, EncodeJson (Record row)) 
    => 
    Jsonable (Record row)

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
