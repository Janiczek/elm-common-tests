module Test.Dict exposing (suite)

import CommonTests
import CommonTests.Dict as CDict
import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


suite : Test
suite =
    CommonTests.isDict (CDict.elmCoreDict Debug.toString)
