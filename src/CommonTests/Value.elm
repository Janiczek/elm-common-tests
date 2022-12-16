module CommonTests.Value exposing (Value(..), fuzzer, map, toString, unwrap)

import Fuzz exposing (Fuzzer)


{-| Good for testing non-comparable values.
-}
type Value
    = Value Int


unwrap : Value -> Int
unwrap (Value n) =
    n


fuzzer : Fuzzer Value
fuzzer =
    Fuzz.map Value Fuzz.int


map : (Int -> Int) -> Value -> Value
map fn (Value n) =
    Value (fn n)


toString : Value -> String
toString (Value n) =
    String.fromInt n
