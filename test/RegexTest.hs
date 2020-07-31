import qualified DataTypesTest
import qualified MatchingTest
import qualified ParserTest

import Test.Framework

tests = [
        DataTypesTest.tests,
        MatchingTest.tests,
        ParserTest.tests
    ]

main = defaultMain tests