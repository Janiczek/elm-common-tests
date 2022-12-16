module Test.Dict exposing (suite)

import CommonTests
import CommonTests.Dict as CDict
import Test exposing (Test)


suite : Test
suite =
    CommonTests.isDict CDict.elmCoreDict
