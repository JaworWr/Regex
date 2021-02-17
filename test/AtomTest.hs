module AtomTest (tests) where


import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Atom

errorMsg = "Incorrect result."

tests = testGroup "Atom" [
    testCase "AtomPredicate Monoid" $ do
        let pr1 = AtomChar 'a'
        let pr2 = AtomChar 'b'
        let pr = pr1 <> pr2 <> mempty

        assertEqual errorMsg pr (AtomOr pr1 pr2)
        assertEqual errorMsg (show pr) "'a','b'"
        
        assertBool errorMsg $ getPredicate pr 'a'
        assertBool errorMsg $ getPredicate pr 'b'
        assertBool errorMsg . not $ getPredicate pr 'c'
    ,
    testCase "Character range" $ do
        let pr1 = makeRange 'a' 'e'
        let pr2 = makeRange 'e' 'e'
        let pr3 = makeRange 'e' 'a'

        assertEqual errorMsg (show <$> pr1) $ Just "'a-e'"
        assertEqual errorMsg (show <$> pr2) $ Just "'e-e'"
        assertEqual errorMsg (show <$> pr3) Nothing

        assertEqual errorMsg (getPredicate <$> pr1 <*> Just 'e') $ Just True
        assertEqual errorMsg (getPredicate <$> pr1 <*> Just 'a') $ Just True
        assertEqual errorMsg (getPredicate <$> pr1 <*> Just 'c') $ Just True
        assertEqual errorMsg (getPredicate <$> pr1 <*> Just 'f') $ Just False
        assertEqual errorMsg (getPredicate <$> pr1 <*> Just '\n') $ Just False
        assertEqual errorMsg (getPredicate <$> pr2 <*> Just 'e') $ Just True
        assertEqual errorMsg (getPredicate <$> pr2 <*> Just 'c') $ Just False
    ]
