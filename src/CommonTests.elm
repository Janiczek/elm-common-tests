module CommonTests exposing (DictAPI, isDict)

{-|

@docs DictAPI, isDict

-}

import CommonTests.Dict as CDict
import CommonTests.Value as Value exposing (Value)
import Dict exposing (Dict)
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


{-| TODO: duplicate the record definition to be visible in package docs?

TODO dict and dict2 differ only in the value: DictAPI (Dict k v) (Dict k v2)

-}
type alias DictAPI d cd k v r =
    CDict.DictAPI d cd k v r


isDict : DictAPI d (Dict String Value) String Value (List Int) -> Test
isDict c =
    CDict.isDict c
