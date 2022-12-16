module CommonTests.Dict.Create exposing (Create(..), all, label)


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
