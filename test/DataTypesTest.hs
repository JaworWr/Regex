module DataTypesTest (tests) where


import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import DataTypes

errorMsg = "Incorrect result."

tests = testGroup "DataTypes" [
    testCase "AtomPredicate Monoid" $ do
        let pr1 = charPredicate 'a'
        let pr2 = charPredicate 'b'
        let pr = pr1 <> pr2 <> mempty

        assertEqual errorMsg (atomPredDesc pr) "'a','b'"
        assertBool errorMsg $ atomPred pr 'a'
        assertBool errorMsg $ atomPred pr 'b'
        assertBool errorMsg . not $ atomPred pr 'c'
    ]