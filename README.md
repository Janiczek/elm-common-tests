# CommonTests

Test your implementations of common data structures with ease with these predefined tests!

All you need to do is to provide a record with your functions, like:

```elm
module Example exposing (..)

import CommonTests
import OpaqueDict
import Test exposing (Test)


suite : Test
suite =
    CommonTests.isDict
        -- Debug.toString
        { dictToString = Debug.toString
        , elmCoreDictToString = Debug.toString

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
```
