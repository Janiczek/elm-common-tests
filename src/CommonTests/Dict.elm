module CommonTests.Dict exposing
    ( DictAPI
    , elmCoreDict
    , isDict
    )

import ArchitectureTest exposing (TestedApp, TestedModel, TestedUpdate)
import CommonTests.Dict.Create as Create exposing (Create(..))
import CommonTests.Dict.Update as Update exposing (Update(..))
import CommonTests.Helpers exposing (kvFuzzer, test, test2, test3)
import CommonTests.Value as Value exposing (Value(..))
import Dict exposing (Dict)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


type alias DictAPI d cd k v r =
    -- Debug.toString
    { dictToString : d -> String
    , elmCoreDictToString : cd -> String

    -- Creation
    , empty : Maybe d
    , singleton : Maybe (k -> v -> d)
    , fromList : Maybe (List ( k, v ) -> d)

    -- Updating
    , insert : Maybe (k -> v -> d -> d)
    , remove : Maybe (k -> d -> d)
    , update : Maybe (k -> (Maybe v -> Maybe v) -> d -> d)
    , map : Maybe ((k -> v -> v) -> d -> d) -- TODO not as general as we could be
    , filter : Maybe ((k -> v -> Bool) -> d -> d)
    , union : Maybe (d -> d -> d)
    , intersect : Maybe (d -> d -> d)
    , diff : Maybe (d -> d -> d) -- TODO not as general as we could be

    -- Querying
    , size : Maybe (d -> Int)
    , isEmpty : Maybe (d -> Bool)
    , member : Maybe (k -> d -> Bool)
    , get : Maybe (k -> d -> Maybe v)
    , toList : Maybe (d -> List ( k, v ))
    , foldl : Maybe ((k -> v -> r -> r) -> r -> d -> r)
    , foldr : Maybe ((k -> v -> r -> r) -> r -> d -> r)
    , partition : Maybe ((k -> v -> Bool) -> d -> ( d, d ))
    , keys : Maybe (d -> List k)
    , values : Maybe (d -> List v)
    , merge :
        -- TODO not as general as we could be
        Maybe
            ((k -> v -> r -> r)
             -> (k -> v -> v -> r -> r)
             -> (k -> v -> r -> r)
             -> d
             -> d
             -> r
             -> r
            )
    }



-- Dict API


elmCoreDict :
    (Dict comparable v -> String)
    -> DictAPI (Dict comparable v) (Dict comparable v) comparable v r
elmCoreDict toString =
    { dictToString = toString
    , elmCoreDictToString = toString
    , empty = Just Dict.empty
    , singleton = Just Dict.singleton
    , insert = Just Dict.insert
    , update = Just Dict.update
    , remove = Just Dict.remove
    , isEmpty = Just Dict.isEmpty
    , member = Just Dict.member
    , get = Just Dict.get
    , size = Just Dict.size
    , keys = Just Dict.keys
    , values = Just Dict.values
    , toList = Just Dict.toList
    , fromList = Just Dict.fromList
    , map = Just Dict.map
    , foldl = Just Dict.foldl
    , foldr = Just Dict.foldr
    , filter = Just Dict.filter
    , partition = Just Dict.partition
    , union = Just Dict.union
    , intersect = Just Dict.intersect
    , diff = Just Dict.diff
    , merge = Just Dict.merge
    }



-- TESTS


isDict : DictAPI d (Dict String Value) String Value r -> Test
isDict c =
    Test.describe "Dict behaviour"
        [ Test.describe "Laws of the Dict API"
            [ emptyLaws c
            , Test.todo "rest of functions"
            ]
        , Test.describe "Conformance to elm/core Dict"
            [ creationConformance c
            , Test.todo "Updating"
            , Test.todo "Querying"
            ]
        ]


emptyLaws : DictAPI d cd String Value r -> Test
emptyLaws c =
    Test.describe "empty"
        [ test2 "isEmpty empty == True" c.empty c.isEmpty <|
            \label empty isEmpty ->
                Test.test label <|
                    \() ->
                        isEmpty empty
                            |> Expect.equal True
        , test3 "isEmpty (insert k v empty) == False" c.empty c.isEmpty c.insert <|
            \label empty isEmpty insert ->
                Test.fuzz kvFuzzer label <|
                    \( k, v ) ->
                        isEmpty (insert k v empty)
                            |> Expect.equal False
        , test2 "toList empty == []" c.empty c.toList <|
            \label empty toList ->
                Test.test label <|
                    \() ->
                        toList empty
                            |> Expect.equalLists []
        , behavesTheSameWhenCreatedVia c
            Fuzz.unit
            (c.empty |> Maybe.map (\empty () -> empty))
            (c.fromList |> Maybe.map (\fromList () -> fromList []))
            "empty behaves exactly like fromList []"
        , behavesTheSameWhenCreatedVia c
            kvFuzzer
            (Maybe.map2 (\insert empty ( k, v ) -> insert k v empty)
                c.insert
                c.empty
            )
            (c.singleton |> Maybe.map (\singleton ( k, v ) -> singleton k v))
            "empty>>insert behaves exactly like singleton"
        ]


{-| Creation: empty, singleton, fromList
-}
creationConformance : DictAPI d (Dict String Value) String Value r -> Test
creationConformance c =
    Create.all
        |> List.map (behavesLikeDictWhenCreatedVia c)
        |> Test.describe "Creation"



-- Tests using random sequences of operations


behavesLikeDictWhenCreatedVia :
    DictAPI d (Dict String Value) String Value r
    -> Create
    -> Test
behavesLikeDictWhenCreatedVia c create =
    test2 (Create.label create)
        c.toList
        (initFuzzer c create)
    <|
        \label toList initFuzzer_ ->
            let
                modelToString : ( d, Dict String Value ) -> String
                modelToString ( dict, coreDict ) =
                    String.join "\n"
                        [ "Tested Dict:   " ++ c.dictToString dict
                        , "elm/core Dict: " ++ c.elmCoreDictToString coreDict
                        ]

                updateDict : Update -> d -> Maybe d
                updateDict u dict =
                    updateWithApi c u dict

                update : Update -> ( d, Dict String Value ) -> ( d, Dict String Value )
                update u ( d, cd ) =
                    Maybe.map2 (\d2 cd2 -> ( d2, cd2 ))
                        (updateDict u d)
                        (updateCoreDict c u cd)
                        |> Maybe.withDefault ( d, cd )

                testedApp : TestedApp ( d, Dict String Value ) Update
                testedApp =
                    { model = ArchitectureTest.FuzzedModel initFuzzer_
                    , update = ArchitectureTest.UpdateWithoutCmds update
                    , msgFuzzer = Update.fuzzer
                    , msgToString = Update.toString
                    , modelToString = modelToString
                    }
            in
            ArchitectureTest.invariantTest label testedApp <|
                \_ _ ( testedDict, coreDict ) ->
                    toList testedDict
                        |> Expect.equalLists (Dict.toList coreDict)


behavesTheSameWhenCreatedVia :
    DictAPI d cd String Value r
    -> Fuzzer a
    -> Maybe (a -> d)
    -> Maybe (a -> d)
    -> String
    -> Test
behavesTheSameWhenCreatedVia c initDataFuzzer toFirst toSecond label =
    test3 label
        c.toList
        toFirst
        toSecond
    <|
        \label_ toList toFirst_ toSecond_ ->
            let
                initFuzzer_ : Fuzzer ( d, d )
                initFuzzer_ =
                    initDataFuzzer
                        |> Fuzz.map
                            (\initData ->
                                ( toFirst_ initData
                                , toSecond_ initData
                                )
                            )

                modelToString : ( d, d ) -> String
                modelToString ( first, second ) =
                    String.join "\n"
                        [ "First:  " ++ c.dictToString first
                        , "Second: " ++ c.dictToString second
                        ]

                updateSingle : Update -> d -> Maybe d
                updateSingle u dict =
                    updateWithApi c u dict

                update : Update -> ( d, d ) -> ( d, d )
                update u ( first, second ) =
                    Maybe.map2 Tuple.pair
                        (updateSingle u first)
                        (updateSingle u second)
                        |> Maybe.withDefault ( first, second )

                testedApp : TestedApp ( d, d ) Update
                testedApp =
                    { model = ArchitectureTest.FuzzedModel initFuzzer_
                    , update = ArchitectureTest.UpdateWithoutCmds update
                    , msgFuzzer = Update.fuzzer
                    , msgToString = Update.toString
                    , modelToString = modelToString
                    }
            in
            ArchitectureTest.invariantTest label_ testedApp <|
                \_ _ ( firstDict, secondDict ) ->
                    toList firstDict
                        |> Expect.equalLists (toList secondDict)



-- CREATE


initFuzzer :
    DictAPI d cd String Value r
    -> Create
    -> Maybe (Fuzzer ( d, Dict String Value ))
initFuzzer c create =
    case create of
        Empty ->
            Maybe.map
                (\empty -> Fuzz.constant ( empty, Dict.empty ))
                c.empty

        Singleton ->
            Maybe.map
                (\singleton ->
                    kvFuzzer
                        |> Fuzz.map
                            (\( k, v ) ->
                                ( singleton k v
                                , Dict.singleton k v
                                )
                            )
                )
                c.singleton

        FromList ->
            Maybe.map
                (\fromList ->
                    Fuzz.list kvFuzzer
                        |> Fuzz.map
                            (\list ->
                                ( fromList list
                                , Dict.fromList list
                                )
                            )
                )
                c.fromList



-- UPDATE


updateWithApi : DictAPI d cd String Value r -> Update -> d -> Maybe d
updateWithApi c update dict =
    case update of
        Insert k v ->
            c.insert |> Maybe.map (\insert -> insert k v dict)

        Remove k ->
            c.remove |> Maybe.map (\remove -> remove k dict)

        UpdateNNJN k ->
            c.update |> Maybe.map (\update_ -> update_ k nothingToNothingJustToNothing dict)

        UpdateNNJJ k ->
            c.update |> Maybe.map (\update_ -> update_ k nothingToNothingJustToJust dict)

        UpdateNJJN k ->
            c.update |> Maybe.map (\update_ -> update_ k nothingToJustJustToNothing dict)

        UpdateNJJJ k ->
            c.update |> Maybe.map (\update_ -> update_ k nothingToJustJustToJust dict)

        Map ->
            c.map |> Maybe.map (\map -> map (\_ v -> Value.map (\n -> n + 1) v) dict)

        Filter ->
            c.filter |> Maybe.map (\filter -> filter (\_ v -> modBy 2 (Value.unwrap v) == 0) dict)

        Union other ->
            Maybe.map2 (\union fromList -> union dict (fromList other))
                c.union
                c.fromList

        Intersect other ->
            Maybe.map2 (\intersect fromList -> intersect dict (fromList other))
                c.intersect
                c.fromList

        Diff other ->
            Maybe.map2 (\diff fromList -> diff dict (fromList other))
                c.diff
                c.fromList


nothingToNothingJustToNothing : Maybe Value -> Maybe Value
nothingToNothingJustToNothing mv =
    case mv of
        Nothing ->
            Nothing

        Just _ ->
            Nothing


nothingToNothingJustToJust : Maybe Value -> Maybe Value
nothingToNothingJustToJust mv =
    case mv of
        Nothing ->
            Nothing

        Just v ->
            Just (Value.map (\n -> n + 1) v)


nothingToJustJustToNothing : Maybe Value -> Maybe Value
nothingToJustJustToNothing mv =
    case mv of
        Nothing ->
            Just (Value 1)

        Just _ ->
            Nothing


nothingToJustJustToJust : Maybe Value -> Maybe Value
nothingToJustJustToJust mv =
    case mv of
        Nothing ->
            Just (Value 1)

        Just v ->
            Just (Value.map (\n -> n + 1) v)


updateCoreDict : DictAPI d (Dict String Value) String Value r -> Update -> Dict String Value -> Maybe (Dict String Value)
updateCoreDict c update dict =
    updateWithApi (elmCoreDict c.elmCoreDictToString) update dict



-- QUERY


type Query
    = Size
    | IsEmpty
    | Member
    | Get
    | Foldl
    | Foldr
    | Partition
    | Merge
    | Keys
    | Values
    | ToList
