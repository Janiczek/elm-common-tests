module CommonTests.Dict.Query exposing (Query(..), fuzzer, label)

import CommonTests.Helpers exposing (keyFuzzer, kvFuzzer, kvToString, listToString)
import CommonTests.Value as Value exposing (Value)
import Fuzz exposing (Fuzzer)


type Query
    = Size
    | IsEmpty
    | Member String
    | Get String
    | FoldlCons (List Int)
    | FoldrCons (List Int)
    | PartitionEvenOdd
    | Merge (List ( String, Value )) (List Int)
    | Keys
    | Values
    | ToList


fuzzer : Fuzzer Query
fuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Size
        , Fuzz.constant IsEmpty
        , Fuzz.map Member keyFuzzer
        , Fuzz.map Get keyFuzzer
        , Fuzz.map FoldlCons (Fuzz.list Fuzz.int)
        , Fuzz.map FoldrCons (Fuzz.list Fuzz.int)
        , Fuzz.constant PartitionEvenOdd
        , Fuzz.map2 Merge (Fuzz.list kvFuzzer) (Fuzz.list Fuzz.int)
        , Fuzz.constant Keys
        , Fuzz.constant Values
        , Fuzz.constant ToList
        ]


label : Query -> String
label query =
    case query of
        Size ->
            "size"

        IsEmpty ->
            "isEmpty"

        Member k ->
            "member \"" ++ k ++ "\""

        Get k ->
            "get \"" ++ k ++ "\""

        FoldlCons init ->
            "foldl consIntValue " ++ listToString String.fromInt init

        FoldrCons init ->
            "foldr consIntValue " ++ listToString String.fromInt init

        PartitionEvenOdd ->
            "partition isValueEven"

        Merge other init ->
            "merge onLeft onBoth onRight this "
                ++ listToString kvToString other
                ++ " "
                ++ listToString String.fromInt init

        Keys ->
            "keys"

        Values ->
            "values"

        ToList ->
            "toList"
