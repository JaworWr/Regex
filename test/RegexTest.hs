import qualified AtomTest
import qualified MatchingTest
import qualified ParserTest

import Test.Framework

tests = [
        AtomTest.tests,
        MatchingTest.tests,
        ParserTest.tests
    ]

main = defaultMain tests