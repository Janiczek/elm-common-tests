module CommonTests.Dict.Query exposing (Query(..), fuzzer)

import CommonTests.Helpers exposing (keyFuzzer, kvFuzzer)
import Fuzz exposing (Fuzzer)


type Query
    = Size
    | IsEmpty
    | Member String
    | Get String
    | FoldlCons (List Int)
    | FoldrCons (List Int)
    | PartitionEvenOdd
    | Merge (List ( String, Int )) (List Int)
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
