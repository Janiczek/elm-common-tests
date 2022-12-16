module CommonTests.Dict.Update exposing (Update(..), fuzzer, toString)

import CommonTests.Helpers exposing (keyFuzzer, kvFuzzer)
import CommonTests.Value as Value exposing (Value)
import Fuzz exposing (Fuzzer)


type Update
    = Insert String Value
    | Remove String
    | Update String -- TODO update all the possible combinations (N->N, N->J, J->N, J->J)
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
        , Fuzz.map Update keyFuzzer
        , Fuzz.constant Map
        , Fuzz.constant Filter
        , Fuzz.map Union (Fuzz.list kvFuzzer)
        , Fuzz.map Intersect (Fuzz.list kvFuzzer)
        , Fuzz.map Diff (Fuzz.list kvFuzzer)
        ]


toString : Update -> String
toString update =
    -- TODO make these look more like function calls
    case update of
        Insert k v ->
            "Insert \"" ++ k ++ "\" (" ++ Value.toString v ++ ")"

        Remove k ->
            "Remove \"" ++ k ++ "\""

        Update k ->
            "Update \"" ++ k ++ "\""

        Map ->
            "Map"

        Filter ->
            "Filter"

        Union list ->
            "Union " ++ listToString list

        Intersect list ->
            "Intersect " ++ listToString list

        Diff list ->
            "Diff " ++ listToString list


listToString : List ( String, Value ) -> String
listToString list =
    list
        |> List.map (\( k, v ) -> "(\"" ++ k ++ "\", " ++ Value.toString v ++ ")")
        |> String.join ", "
