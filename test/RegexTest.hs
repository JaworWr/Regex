import qualified MatchingTest
import qualified ParserTest

import Test.Framework

tests = [
        MatchingTest.tests,
        ParserTest.tests
    ]

main = defaultMain tests