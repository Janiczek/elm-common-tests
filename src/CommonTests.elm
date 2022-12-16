module CommonTests exposing (isDict, DictAPI)

{-|

@docs isDict, DictAPI

-}

import CommonTests.Dict as CDict
import Dict exposing (Dict)
import Test exposing (Test)


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
    CDict.isDict c


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
    , map : Maybe ((String -> Int -> Int) -> d -> d)
    , filter : Maybe ((String -> Int -> Bool) -> d -> d)
    , union : Maybe (d -> d -> d)
    , intersect : Maybe (d -> d -> d)
    , diff : Maybe (d -> d -> d)

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
