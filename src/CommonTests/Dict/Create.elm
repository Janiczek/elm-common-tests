module CommonTests.Dict.Create exposing (Create(..), all, label)

import Fuzz exposing (Fuzzer)


type Create
    = Empty
    | Singleton
    | FromList


all : List Create
all =
    [ Empty
    , Singleton
    , FromList
    ]


label : Create -> String
label create =
    case create of
        Empty ->
            "empty"

        Singleton ->
            "singleton"

        FromList ->
            "fromList"
