module Test.Dict exposing (suite)

import CommonTests.Dict
import CommonTests.Helpers exposing (elmCoreDictToString)
import Dict
import Test exposing (Test)


suite : Test
suite =
    CommonTests.Dict.isDict
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
