module CommonTests.Helpers exposing
    ( test, test2
    , keyFuzzer, valueFuzzer, kvFuzzer
    , listToString, kvToString
    , elmCoreDictToString
    )

{-|

@docs test, test2
@docs keyFuzzer, valueFuzzer, kvFuzzer
@docs listToString, kvToString
@docs elmCoreDictToString

-}

import Dict exposing (Dict)
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


keyFuzzer : Fuzzer String
keyFuzzer =
    Fuzz.asciiChar
        |> Fuzz.map String.fromChar


valueFuzzer : Fuzzer Int
valueFuzzer =
    Fuzz.int


kvFuzzer : Fuzzer ( String, Int )
kvFuzzer =
    Fuzz.pair keyFuzzer valueFuzzer


kvToString : ( String, Int ) -> String
kvToString ( k, v ) =
    "(\"" ++ k ++ "\", " ++ String.fromInt v ++ ")"


listToString : (x -> String) -> List x -> String
listToString valueToString list =
    list
        |> List.map valueToString
        |> String.join ", "
        |> (\s -> "[" ++ s ++ "]")


elmCoreDictToString : Dict String Int -> String
elmCoreDictToString dict =
    "Dict.fromList "
        ++ listToString
            (\( k, v ) -> "(\"" ++ k ++ "\", " ++ String.fromInt v ++ ")")
            (Dict.toList dict)
