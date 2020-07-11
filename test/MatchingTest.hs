module MatchingTest (tests) where

import Control.Monad

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import DataTypes
import Matching

atom :: Char -> Regex
atom c = Atom (AtomPredicate (== c) [c])

findAllReError = "Incorrect findAllRe result."

tests = testGroup "Matching" [
    testCase "Atom" $ do
        let re = atom 'a'
        assertEqual findAllReError [] $ findAllRe re ""
        assertEqual findAllReError [] $ findAllRe re "bcd"
        assertEqual findAllReError [Matching 1 2] $ findAllRe re "bacd"
        assertEqual findAllReError [Matching 0 1] $ findAllRe re "abcd"
        assertEqual findAllReError [Matching 3 4] $ findAllRe re "bcda"
    ,
    testCase "Multiple Atoms" $ do
        let re = Atom (AtomPredicate (`elem` "abc") "abc")
        assertEqual findAllReError [] $ findAllRe re ""
        assertEqual findAllReError [] $ findAllRe re "def"
        assertEqual findAllReError [Matching 0 1, Matching 2 3, Matching 4 5] $ findAllRe re "adbec"
    ,
    testCase "Wildcard" $ do
        let re = Atom (AtomPredicate (const True) "Wildcard")
        assertEqual findAllReError [] $ findAllRe re ""
        assertEqual findAllReError [Matching 0 1, Matching 1 2] $ findAllRe re "ab"
    ,
    testCase "BOS" $ do
        let re = BOS
        assertEqual findAllReError [Matching 0 0] $ findAllRe re ""
        assertEqual findAllReError [Matching 0 0] $ findAllRe re "bc"
    ,
    testCase "EOS" $ do
        let re = EOS
        assertEqual findAllReError [Matching 0 0] $ findAllRe re ""
        assertEqual findAllReError [Matching 2 2] $ findAllRe re "bc"
    ,
    testCase "Concat" $ do
        let re = Concat (atom 'a') (atom 'b')
        assertEqual findAllReError [] $ findAllRe re ""
        assertEqual findAllReError [Matching 1 3, Matching 4 6] $ findAllRe re "cabdabe"
        assertEqual findAllReError [] $ findAllRe re "cbdade"
    ,
    testCase "BOS+Concat" $ do
        let re = Concat BOS (Concat (atom 'a') (atom 'b'))
        assertEqual findAllReError [] $ findAllRe re "cabde"
        assertEqual findAllReError [Matching 0 2] $ findAllRe re "abcdabeab"
    ,
    testCase "EOS+Concat" $ do
        let re = Concat (Concat (atom 'a') (atom 'b')) EOS
        assertEqual findAllReError [] $ findAllRe re "cabde"
        assertEqual findAllReError [Matching 7 9] $ findAllRe re "abcdabeab"
    ,
    testCase "Or" $ do
        let re = Or (atom 'a') (atom 'b')
        assertEqual findAllReError [] $ findAllRe re ""
        assertEqual findAllReError [] $ findAllRe re "cdefg"

        let res = [Matching 0 1, Matching 2 3, Matching 4 5, Matching 6 7]
        assertEqual findAllReError res $ findAllRe re "acbdaeb"
    ,
    testGroup "Repeat" (
        map (\e -> testCase ("{0, 0} " ++ show e) $ do
            let re = Repeat 0 (Just 0) e (atom 'a')
            assertEqual findAllReError [Matching 0 0] $ findAllRe re ""    
            assertEqual findAllReError [Matching 0 0, Matching 1 1, Matching 2 2] $ findAllRe re "ab"    
        ) [Lazy, Eager] ++

        map (\e -> testCase ("{1, 0} " ++ show e) $ do
            let re = Repeat 1 (Just 0) e (atom 'a')
            assertEqual findAllReError [] $ findAllRe re ""    
            assertEqual findAllReError [] $ findAllRe re "ab"    
        ) [Lazy, Eager] ++

        map (\e -> testCase ("{2, 2} " ++ show e) $ do
            let re = Repeat 2 (Just 2) e (atom 'a')
            assertEqual findAllReError [] $ findAllRe re ""    
            assertEqual findAllReError [Matching 0 2, Matching 1 3] $ findAllRe re "aaa"    
        ) [Lazy, Eager] ++

        [
            testCase "{2, 4} Eager" $ do
                let re = Repeat 2 (Just 4) Eager (atom 'a')
                let res = [Matching 1 5, Matching 2 6, Matching 3 6, Matching 4 6, Matching 9 12, Matching 10 12]
                assertEqual findAllReError res $ findAllRe re "baaaaacadaaae"
                assertEqual findAllReError [] $ findAllRe re ""
            ,
            testCase "{2, 4} Lazy" $ do
                let re = Repeat 2 (Just 4) Lazy (atom 'a')
                let res = [Matching 1 3, Matching 2 4, Matching 3 5, Matching 4 6, Matching 9 11, Matching 10 12]
                assertEqual findAllReError res $ findAllRe re "baaaaacadaaae"
                assertEqual findAllReError [] $ findAllRe re ""
            ,
            testCase "{2,} Eager" $ do
                let re = Repeat 2 Nothing Eager (atom 'a')
                assertEqual findAllReError [] $ findAllRe re ""
                assertEqual findAllReError [Matching 0 3, Matching 1 3] $ findAllRe re "aaa"
            ,
            testCase "{2,} Lazy" $ do
                let re = Repeat 2 Nothing Lazy (atom 'a')
                assertEqual findAllReError [] $ findAllRe re ""
                assertEqual findAllReError [Matching 0 2, Matching 1 3] $ findAllRe re "aaa"
        ] ++
        
        map (\e -> testCase ("{1, 4}+Concat " ++ show e) $ do
            let re = Concat (atom 'b') $ Concat (Repeat 1 (Just 4) e (atom 'a')) (atom 'c')
            assertEqual findAllReError [Matching 1 6] $ findAllRe re "ebaaacd"
        ) [Lazy, Eager]
    )
    ,
    testCase "Overlapping" $ do
        let re = Concat (atom 'a') $ Concat (atom 'b') (atom 'a')
        assertEqual findAllReError [Matching 0 3, Matching 2 5, Matching 4 7] $ findAllRe re "abababa"
    ,
    testCase "Nonoverlapping" $ do
        let re = Concat (atom 'a') $ Concat (atom 'b') (atom 'a')
        assertEqual "Incorrect findNonoverlappingRe result"
            [Matching 0 3, Matching 4 7] $ findNonoverlappingRe re "abababa"
    ]
