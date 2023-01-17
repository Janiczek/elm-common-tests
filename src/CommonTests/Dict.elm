module CommonTests.Dict exposing
    ( DictAPI
    , isDict
    , isUnorderedDict
    )

import ArchitectureTest exposing (TestedApp)
import CommonTests.Dict.Create as Create exposing (Create(..))
import CommonTests.Dict.Query as Query exposing (Query(..))
import CommonTests.Dict.Update as Update exposing (Update(..))
import CommonTests.Helpers exposing (elmCoreDictToString, kvFuzzer, test, test2)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Maybe.Extra as Maybe
import Test exposing (Test)


{-| Provide your functions. Typically something like the below example.

Note: you can opt out of testing some functions with `Nothing`, but beware: not
providing eg. `toList` will result in almost no tests being actually ran.

    -- Debug.toString
    { dictToString = Debug.toString

    -- Creation
    , empty = Just <| OpaqueDict.empty identity
    , singleton = Just <| OpaqueDict.singleton identity
    , fromList = Just <| OpaqueDict.fromList identity

    -- Updating
    , insert = Just OpaqueDict.insert
    , update = Just OpaqueDict.update
    , remove = Just OpaqueDict.remove
    , map = Just OpaqueDict.map
    , filter = Just OpaqueDict.filter
    , union = Just OpaqueDict.union
    , intersect = Just OpaqueDict.intersect
    , diff = Just OpaqueDict.diff

    -- Querying
    , size = Just OpaqueDict.size
    , isEmpty = Just OpaqueDict.isEmpty
    , member = Just OpaqueDict.member
    , get = Just OpaqueDict.get
    , toList = Just OpaqueDict.toList
    , foldl = Just OpaqueDict.foldl
    , foldr = Just OpaqueDict.foldr
    , partition = Just OpaqueDict.partition
    , keys = Just OpaqueDict.keys
    , values = Just OpaqueDict.values
    , merge = Just OpaqueDict.merge
    }

-}
type alias DictAPI d =
    -- Debug.toString
    { dictToString : d -> String

    -- Creation
    , empty : Maybe d
    , singleton : Maybe (String -> Int -> d)
    , fromList : Maybe (List ( String, Int ) -> d)

    -- Updating
    , insert : Maybe (String -> Int -> d -> d)
    , remove : Maybe (String -> d -> d)
    , update : Maybe (String -> (Maybe Int -> Maybe Int) -> d -> d)
    , map : Maybe ((String -> Int -> Int) -> d -> d) -- TODO not as general as we could be
    , filter : Maybe ((String -> Int -> Bool) -> d -> d)
    , union : Maybe (d -> d -> d)
    , intersect : Maybe (d -> d -> d)
    , diff : Maybe (d -> d -> d) -- TODO not as general as we could be

    -- Querying
    , size : Maybe (d -> Int)
    , isEmpty : Maybe (d -> Bool)
    , member : Maybe (String -> d -> Bool)
    , get : Maybe (String -> d -> Maybe Int)
    , toList : Maybe (d -> List ( String, Int ))
    , foldl : Maybe ((String -> Int -> List Int -> List Int) -> List Int -> d -> List Int)
    , foldr : Maybe ((String -> Int -> List Int -> List Int) -> List Int -> d -> List Int)
    , partition : Maybe ((String -> Int -> Bool) -> d -> ( d, d ))
    , keys : Maybe (d -> List String)
    , values : Maybe (d -> List Int)
    , merge :
        -- TODO not as general as we could be
        Maybe
            ((String -> Int -> List Int -> List Int)
             -> (String -> Int -> Int -> List Int -> List Int)
             -> (String -> Int -> List Int -> List Int)
             -> d
             -> d
             -> List Int
             -> List Int
            )
    }



-- Dict API


elmCoreDict : DictAPI (Dict String Int)
elmCoreDict =
    { dictToString = elmCoreDictToString
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


{-| Given a record of your Dict functions, this function will run a
comprehensive suite of tests against them, comparing them to the elm/core Dict.

It will:

  - Create the initial Dict using a random constructor (like `empty`, `singleton` or `fromList`)
  - Run a random sequence of "updating" actions on it (like `insert`, `filter`, `diff` etc.)
  - Query the final Dict using a random function (like `size`, `get`, `partition`, `toList` etc.)

The process above will run on both _your_ Dict and the `elm/core` Dict in
tandem. (The same commands will run for both.)

There should be no visible difference in behaviour: eg. order of items given
from `toList` should be the same.

This helps prevent subtle surprises when users switch between Dict
implementations, like `update key (\_ -> Nothing)` not removing items from the
Dict (a real bug in one of the 3rd-party Dicts!).

-}
isDict : DictAPI d -> Test
isDict c =
    Test.describe "Conformance to elm/core Dict"
        [ creationConformance Ordered c
        , queryingConformance Ordered c

        {- Conformance for updating functions (like `map`, `insert` etc.)
           is already tested thanks to using
           `Update.fuzzer` in all the ArchitectureTest invocations.
        -}
        ]


{-| Same as `isDict`, but doesn't care about the order of lists gathered from
query Dict functions like `keys`, `toList`, `partition` etc.

(Note folds on unordered lists might also behave differently if your combining
function is not commutative, given the order is not guaranteed. As such, this
test suite will only check that all items are used in the fold.)

-}
isUnorderedDict : DictAPI d -> Test
isUnorderedDict c =
    Test.describe "Conformance to elm/core Dict (unordered)"
        [ creationConformance Unordered c
        , queryingConformance Unordered c

        {- Conformance for updating functions (like `map`, `insert` etc.)
           is already tested thanks to using
           `Update.fuzzer` in all the ArchitectureTest invocations.
        -}
        ]


type Ordering
    = Ordered
    | Unordered


expectEqualLists : Ordering -> List comparable -> List comparable -> Expectation
expectEqualLists ordering =
    case ordering of
        Unordered ->
            \l1 l2 -> Expect.equal (List.sort l2) (List.sort l1)

        Ordered ->
            \l1 l2 -> Expect.equal l2 l1


expectEqualPartitions : Ordering -> ( List comparable, List comparable ) -> ( List comparable, List comparable ) -> Expectation
expectEqualPartitions ordering =
    case ordering of
        Unordered ->
            \( p1l1, p1l2 ) ( p2l1, p2l2 ) ->
                Expect.equal
                    ( List.sort p2l1
                    , List.sort p2l2
                    )
                    ( List.sort p1l1
                    , List.sort p1l2
                    )

        Ordered ->
            \p1 p2 -> Expect.equal p2 p1


creationConformance : Ordering -> DictAPI d -> Test
creationConformance ordering c =
    Create.all
        |> List.map (behavesLikeDictWhenCreatedVia ordering c)
        |> Test.describe "Creation"


queryingConformance : Ordering -> DictAPI d -> Test
queryingConformance ordering c =
    test "Querying" (initFuzzerUsingAll c) <|
        \label initFuzzer_ ->
            let
                updateDict : Update -> d -> Maybe d
                updateDict u dict =
                    updateWithApi c u dict

                update : Update -> ( d, Dict String Int ) -> ( d, Dict String Int )
                update u ( d, cd ) =
                    Maybe.map2 (\d2 cd2 -> ( d2, cd2 ))
                        (updateDict u d)
                        (updateCoreDict u cd)
                        |> Maybe.withDefault ( d, cd )

                testedApp : TestedApp ( d, Dict String Int ) Update
                testedApp =
                    { model = ArchitectureTest.FuzzedModel initFuzzer_
                    , update = ArchitectureTest.UpdateWithoutCmds update
                    , msgFuzzer = Update.fuzzer
                    , msgToString = Update.toString
                    , modelToString = dictAndCoreDictToString c
                    }

                modelFuzzer : Fuzzer ( d, Dict String Int )
                modelFuzzer =
                    ArchitectureTest.modelFuzzer testedApp
            in
            Test.fuzz2 Query.fuzzer modelFuzzer label <|
                \query ( testedDict, coreDict ) ->
                    case queryToExpectation ordering c query of
                        Nothing ->
                            {- We're passing because we don't want non-implemented Dict APIs to cause failures.
                               We're only testing their behaviour if those functions are present.
                               Unfortunately there is no Expect.skip and Expect.pass doesn't let us give a reason.
                               So we skip it silently.
                            -}
                            Expect.pass

                        Just toExpectation ->
                            toExpectation testedDict coreDict



-- Tests using random sequences of operations


behavesLikeDictWhenCreatedVia : Ordering -> DictAPI d -> Create -> Test
behavesLikeDictWhenCreatedVia ordering c create =
    test2 (Create.label create)
        c.toList
        (initFuzzer c create)
    <|
        \label toList initFuzzer_ ->
            let
                updateDict : Update -> d -> Maybe d
                updateDict u dict =
                    updateWithApi c u dict

                update : Update -> ( d, Dict String Int ) -> ( d, Dict String Int )
                update u ( d, cd ) =
                    Maybe.map2 (\d2 cd2 -> ( d2, cd2 ))
                        (updateDict u d)
                        (updateCoreDict u cd)
                        |> Maybe.withDefault ( d, cd )

                testedApp : TestedApp ( d, Dict String Int ) Update
                testedApp =
                    { model = ArchitectureTest.FuzzedModel initFuzzer_
                    , update = ArchitectureTest.UpdateWithoutCmds update
                    , msgFuzzer = Update.fuzzer
                    , msgToString = Update.toString
                    , modelToString = dictAndCoreDictToString c
                    }
            in
            ArchitectureTest.invariantTest label testedApp <|
                \_ _ ( testedDict, coreDict ) ->
                    toList testedDict
                        |> expectEqualLists ordering (Dict.toList coreDict)


dictAndCoreDictToString : DictAPI d -> ( d, Dict String Int ) -> String
dictAndCoreDictToString c ( dict, coreDict ) =
    String.join "\n"
        [ "Tested Dict:   " ++ c.dictToString dict
        , "elm/core Dict: " ++ elmCoreDictToString coreDict
        ]



-- CREATE


initFuzzerUsingAll : DictAPI d -> Maybe (Fuzzer ( d, Dict String Int ))
initFuzzerUsingAll c =
    Create.all
        |> Maybe.traverse (initFuzzer c)
        |> Maybe.map Fuzz.oneOf


initFuzzer : DictAPI d -> Create -> Maybe (Fuzzer ( d, Dict String Int ))
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


updateWithApi : DictAPI d -> Update -> d -> Maybe d
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
            c.map |> Maybe.map (\map -> map (\_ v -> v + 1) dict)

        Filter ->
            c.filter |> Maybe.map (\filter -> filter (\_ v -> modBy 2 v == 0) dict)

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


nothingToNothingJustToNothing : Maybe Int -> Maybe Int
nothingToNothingJustToNothing _ =
    Nothing


nothingToNothingJustToJust : Maybe Int -> Maybe Int
nothingToNothingJustToJust mv =
    case mv of
        Nothing ->
            Nothing

        Just v ->
            Just (v + 1)


nothingToJustJustToNothing : Maybe Int -> Maybe Int
nothingToJustJustToNothing mv =
    case mv of
        Nothing ->
            Just 1

        Just _ ->
            Nothing


nothingToJustJustToJust : Maybe Int -> Maybe Int
nothingToJustJustToJust mv =
    case mv of
        Nothing ->
            Just 1

        Just v ->
            Just (v + 1)


updateCoreDict : Update -> Dict String Int -> Maybe (Dict String Int)
updateCoreDict update dict =
    updateWithApi elmCoreDict update dict



-- QUERY


queryToExpectation : Ordering -> DictAPI d -> Query -> Maybe (d -> Dict String Int -> Expectation)
queryToExpectation ordering c query =
    let
        go0 :
            (DictAPI d -> Maybe (d -> a))
            -> (Dict String Int -> a)
            -> Maybe (d -> Dict String Int -> Expectation)
        go0 dictFnGetter coreDictFn =
            dictFnGetter c
                |> Maybe.map
                    (\dictFn dict coreDict ->
                        dictFn dict
                            |> Expect.equal (coreDictFn coreDict)
                    )

        go0List :
            (DictAPI d -> Maybe (d -> List comparable))
            -> (Dict String Int -> List comparable)
            -> Maybe (d -> Dict String Int -> Expectation)
        go0List dictFnGetter coreDictFn =
            dictFnGetter c
                |> Maybe.map
                    (\dictFn dict coreDict ->
                        dictFn dict
                            |> expectEqualLists ordering (coreDictFn coreDict)
                    )

        go1 :
            (DictAPI d -> Maybe (x1 -> d -> a))
            -> (x1 -> Dict String Int -> a)
            -> x1
            -> Maybe (d -> Dict String Int -> Expectation)
        go1 dictFnGetter coreDictFn arg1 =
            dictFnGetter c
                |> Maybe.map
                    (\dictFn dict coreDict ->
                        dictFn arg1 dict
                            |> Expect.equal (coreDictFn arg1 coreDict)
                    )

        goFold :
            (DictAPI d -> Maybe (x1 -> x2 -> d -> List comparable))
            -> (x1 -> x2 -> Dict String Int -> List comparable)
            -> x1
            -> x2
            -> Maybe (d -> Dict String Int -> Expectation)
        goFold dictFnGetter coreDictFn arg1 arg2 =
            dictFnGetter c
                |> Maybe.map
                    (\dictFn dict coreDict ->
                        dictFn arg1 arg2 dict
                            |> expectEqualLists ordering (coreDictFn arg1 arg2 coreDict)
                    )

        {- This would normally be go1, but we need to convert the result of
           Dict.partition before putting it through Expect.equal.

           This is not an issue with other operations since they return a type
           that's not the dict itself. Eg. Dict.member returns a Bool.

           Partition, on the other hand, for a Dict type `d`, returns `(d,d)`.
           And so we get (d,d) from the tested Dict and (Dict String Int,
           Dict String Int) from the core Dict. Those can't be equated.
        -}
        goPartition :
            (String -> Int -> Bool)
            -> Maybe (d -> Dict String Int -> Expectation)
        goPartition arg1 =
            Maybe.map2
                (\partition toList dict coreDict ->
                    partition arg1 dict
                        |> Tuple.mapBoth toList toList
                        |> expectEqualPartitions ordering
                            (Dict.partition arg1 coreDict
                                |> Tuple.mapBoth Dict.toList Dict.toList
                            )
                )
                c.partition
                c.toList

        {- This would normally be go5, but we need to reorder the arguments
           a bit as well: it's not as simple as x1 -> x2 -> d -> a.

           In Dict.merge the init (a) comes as a last argument, which is
           different to all the other tested functions where the dict comes as
           a last argument.

           In other words, the Dict.merge function is not pipelinable, and
           this special helper deals with that.

           Taking advantage of the fact that we know what function we'll use,
           we can get rid of some of the arguments as well that we'd normally
           have to provide to goN.
        -}
        goMerge :
            (String -> Int -> List Int -> List Int)
            -> (String -> Int -> Int -> List Int -> List Int)
            -> (String -> Int -> List Int -> List Int)
            -> List ( String, Int ) -- common representation to both dict implementations
            -> List Int
            -> Maybe (d -> Dict String Int -> Expectation)
        goMerge xLeft xBoth xRight other init =
            Maybe.map2
                (\merge fromList dict coreDict ->
                    merge xLeft xBoth xRight dict (fromList other) init
                        |> expectEqualLists ordering (Dict.merge xLeft xBoth xRight coreDict (Dict.fromList other) init)
                )
                c.merge
                c.fromList
    in
    case query of
        Size ->
            go0 .size Dict.size

        IsEmpty ->
            go0 .isEmpty Dict.isEmpty

        Member k ->
            go1 .member Dict.member k

        Get k ->
            go1 .get Dict.get k

        FoldlCons init ->
            goFold .foldl Dict.foldl (\_ v acc -> v :: acc) init

        FoldrCons init ->
            goFold .foldr Dict.foldr (\_ v acc -> v :: acc) init

        PartitionEvenOdd ->
            goPartition (\_ v -> modBy 2 v == 0)

        Merge other init ->
            goMerge
                (\_ v1 acc -> v1 * 100 :: acc)
                (\_ v1 v2 acc -> (v1 + v2) * 1000 :: acc)
                (\_ v2 acc -> v2 * 10000 :: acc)
                other
                init

        Keys ->
            go0List .keys Dict.keys

        Values ->
            go0List .values Dict.values

        ToList ->
            go0List .toList Dict.toList
