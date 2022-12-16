module CommonTests.Helpers exposing
    ( test, test2, test3
    , withDefaultTest
    , keyFuzzer, kvFuzzer
    )

{-|

@docs test, test2, test3
@docs withDefaultTest
@docs keyFuzzer, kvFuzzer

-}

import CommonTests.Value as Value exposing (Value)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


suffix : String
suffix =
    " - SKIPPED - missing functions in CommonTests.DictAPI"


withDefaultTest : String -> Maybe Test -> Test
withDefaultTest label maybeTest =
    Maybe.withDefault
        (Test.test (label ++ suffix) (\() -> Expect.pass))
        maybeTest


test : String -> Maybe a -> (String -> a -> Test) -> Test
test label x fn =
    Maybe.map (fn label) x
        |> withDefaultTest label


test2 : String -> Maybe a -> Maybe b -> (String -> a -> b -> Test) -> Test
test2 label x1 x2 fn =
    Maybe.map2 (fn label) x1 x2
        |> withDefaultTest label


test3 : String -> Maybe a -> Maybe b -> Maybe c -> (String -> a -> b -> c -> Test) -> Test
test3 label x1 x2 x3 fn =
    Maybe.map3 (fn label) x1 x2 x3
        |> withDefaultTest label


keyFuzzer : Fuzzer String
keyFuzzer =
    Fuzz.asciiChar
        |> Fuzz.map String.fromChar


kvFuzzer : Fuzzer ( String, Value )
kvFuzzer =
    Fuzz.pair keyFuzzer Value.fuzzer
