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
    ,
    testCase "Character range" $ do
        let pr1 = rangePredicate 'a' 'e'
        let pr2 = rangePredicate 'e' 'e'
        let pr3 = rangePredicate 'e' 'a'

        assertEqual errorMsg (atomPredDesc <$> pr1) $ Just "'a-e'"
        assertEqual errorMsg (atomPredDesc <$> pr2) $ Just "'e-e'"
        assertEqual errorMsg (atomPredDesc <$> pr3) Nothing

        assertEqual errorMsg (atomPred <$> pr1 <*> Just 'e') $ Just True
        assertEqual errorMsg (atomPred <$> pr1 <*> Just 'a') $ Just True
        assertEqual errorMsg (atomPred <$> pr1 <*> Just 'c') $ Just True
        assertEqual errorMsg (atomPred <$> pr1 <*> Just 'f') $ Just False
        assertEqual errorMsg (atomPred <$> pr1 <*> Just '\n') $ Just False
        assertEqual errorMsg (atomPred <$> pr2 <*> Just 'e') $ Just True
        assertEqual errorMsg (atomPred <$> pr2 <*> Just 'c') $ Just False
    ]
