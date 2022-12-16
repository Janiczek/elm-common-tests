module CommonTests.Dict.Update exposing (Update(..), fuzzer, toString)

import CommonTests.Helpers exposing (keyFuzzer, kvFuzzer)
import CommonTests.Value as Value exposing (Value)
import Fuzz exposing (Fuzzer)


type Update
    = Insert String Value
    | Remove String
    | UpdateNNJN String -- Nothing to Nothing, Just to Nothing
    | UpdateNNJJ String -- etc.
    | UpdateNJJN String
    | UpdateNJJJ String
    | Map
    | Filter
    | Union (List ( String, Value ))
    | Intersect (List ( String, Value ))
    | Diff (List ( String, Value ))


fuzzer : Fuzzer Update
fuzzer =
    Fuzz.oneOf
        [ Fuzz.map2 Insert keyFuzzer Value.fuzzer
        , Fuzz.map Remove keyFuzzer
        , Fuzz.map UpdateNNJN keyFuzzer
        , Fuzz.map UpdateNNJJ keyFuzzer
        , Fuzz.map UpdateNJJN keyFuzzer
        , Fuzz.map UpdateNJJJ keyFuzzer
        , Fuzz.constant Map
        , Fuzz.constant Filter
        , Fuzz.map Union (Fuzz.list kvFuzzer)
        , Fuzz.map Intersect (Fuzz.list kvFuzzer)
        , Fuzz.map Diff (Fuzz.list kvFuzzer)
        ]


toString : Update -> String
toString update =
    case update of
        Insert k v ->
            "insert \"" ++ k ++ "\" " ++ Value.toString v

        Remove k ->
            "remove \"" ++ k ++ "\""

        UpdateNNJN k ->
            "update \"" ++ k ++ "\" nothingToNothingJustToNothing"

        UpdateNNJJ k ->
            "update \"" ++ k ++ "\" nothingToNothingJustToJust"

        UpdateNJJN k ->
            "update \"" ++ k ++ "\" nothingToJustJustToNothing"

        UpdateNJJJ k ->
            "update \"" ++ k ++ "\" nothingToJustJustToJust"

        Map ->
            "map (\\_ v -> v + 1)"

        Filter ->
            "filter (\\_ v -> modBy 2 v == 0)"

        Union list ->
            "union d (fromList " ++ listToString list ++ ")"

        Intersect list ->
            "intersect d (fromList " ++ listToString list ++ ")"

        Diff list ->
            "diff d (fromList " ++ listToString list ++ ")"


listToString : List ( String, Value ) -> String
listToString list =
    list
        |> List.map (\( k, v ) -> "(\"" ++ k ++ "\", " ++ Value.toString v ++ ")")
        |> String.join ", "
