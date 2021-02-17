module MatchingTest (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import DataTypes
import Matching
import Atom

atom :: Char -> Regex
atom = Atom . AtomChar

errorMsg = "Incorrect result."

tests = testGroup "Matching" [
    testCase "Atom" $ do
        let re = atom 'a'
        assertEqual errorMsg [] $ findAllRe re ""
        assertEqual errorMsg [] $ findAllRe re "bcd"
        assertEqual errorMsg [Matching 1 2] $ findAllRe re "bacd"
        assertEqual errorMsg [Matching 0 1] $ findAllRe re "abcd"
        assertEqual errorMsg [Matching 3 4] $ findAllRe re "bcda"
    ,
    testCase "NegatedAtom" $ do
        let re = Atom . AtomNot $ AtomChar 'a'
        assertEqual errorMsg [] $ findAllRe re ""
        assertEqual errorMsg [Matching 0 1, Matching 1 2, Matching 2 3] $ findAllRe re "bcd"
        assertEqual errorMsg [Matching 0 1, Matching 2 3, Matching 3 4] $ findAllRe re "bacd"
    ,
    testCase "Multiple Atoms" $ do
        let re = Atom (mconcat $ map AtomChar "abc")
        assertEqual errorMsg [] $ findAllRe re ""
        assertEqual errorMsg [] $ findAllRe re "def"
        assertEqual errorMsg [Matching 0 1, Matching 2 3, Matching 4 5] $ findAllRe re "adbec"
    ,
    testCase "Wildcard" $ do
        let re = Atom AtomWildcard
        assertEqual errorMsg [] $ findAllRe re ""
        assertEqual errorMsg [Matching 0 1, Matching 1 2] $ findAllRe re "ab"
    ,
    testCase "BOS" $ do
        let re = BOS
        assertEqual errorMsg [Matching 0 0] $ findAllRe re ""
        assertEqual errorMsg [Matching 0 0] $ findAllRe re "bc"
    ,
    testCase "EOS" $ do
        let re = EOS
        assertEqual errorMsg [Matching 0 0] $ findAllRe re ""
        assertEqual errorMsg [Matching 2 2] $ findAllRe re "bc"
    ,
    testCase "Concat" $ do
        let re = Concat (atom 'a') (atom 'b')
        assertEqual errorMsg [] $ findAllRe re ""
        assertEqual errorMsg [Matching 1 3, Matching 4 6] $ findAllRe re "cabdabe"
        assertEqual errorMsg [] $ findAllRe re "cbdade"
    ,
    testCase "BOS+Concat" $ do
        let re = Concat BOS (Concat (atom 'a') (atom 'b'))
        assertEqual errorMsg [] $ findAllRe re "cabde"
        assertEqual errorMsg [Matching 0 2] $ findAllRe re "abcdabeab"
    ,
    testCase "EOS+Concat" $ do
        let re = Concat (Concat (atom 'a') (atom 'b')) EOS
        assertEqual errorMsg [] $ findAllRe re "cabde"
        assertEqual errorMsg [Matching 7 9] $ findAllRe re "abcdabeab"
    ,
    testCase "Or" $ do
        let re = Or (atom 'a') (atom 'b')
        assertEqual errorMsg [] $ findAllRe re ""
        assertEqual errorMsg [] $ findAllRe re "cdefg"

        let res = [Matching 0 1, Matching 2 3, Matching 4 5, Matching 6 7]
        assertEqual errorMsg res $ findAllRe re "acbdaeb"
    ,
    testGroup "Repeat" (
        map (\e -> testCase ("{0, 0} " ++ show e) $ do
            let re = Repeat 0 (Just 0) e (atom 'a')
            assertEqual errorMsg [Matching 0 0] $ findAllPosRe re ""    
            assertEqual errorMsg [Matching 0 0, Matching 1 1, Matching 2 2] $ findAllPosRe re "ab"    
        ) [Lazy, Eager] ++

        map (\e -> testCase ("{1, 0} " ++ show e) $ do
            let re = Repeat 1 (Just 0) e (atom 'a')
            assertEqual errorMsg [] $ findAllPosRe re ""    
            assertEqual errorMsg [] $ findAllPosRe re "ab"    
        ) [Lazy, Eager] ++

        map (\e -> testCase ("{2, 2} " ++ show e) $ do
            let re = Repeat 2 (Just 2) e (atom 'a')
            assertEqual errorMsg [] $ findAllPosRe re ""    
            assertEqual errorMsg [Matching 0 2, Matching 1 3] $ findAllPosRe re "aaa"    
        ) [Lazy, Eager] ++

        [
            testCase "{2, 4} Eager" $ do
                let re = Repeat 2 (Just 4) Eager (atom 'a')
                let res = [Matching 1 5, Matching 2 6, Matching 3 6, Matching 4 6, Matching 9 12, Matching 10 12]
                assertEqual errorMsg res $ findAllPosRe re "baaaaacadaaae"
                assertEqual errorMsg [] $ findAllPosRe re ""
            ,
            testCase "{2, 4} Lazy" $ do
                let re = Repeat 2 (Just 4) Lazy (atom 'a')
                let res = [Matching 1 3, Matching 2 4, Matching 3 5, Matching 4 6, Matching 9 11, Matching 10 12]
                assertEqual errorMsg res $ findAllPosRe re "baaaaacadaaae"
                assertEqual errorMsg [] $ findAllPosRe re ""
            ,
            testCase "{2,} Eager" $ do
                let re = Repeat 2 Nothing Eager (atom 'a')
                assertEqual errorMsg [] $ findAllPosRe re ""
                assertEqual errorMsg [Matching 0 3, Matching 1 3] $ findAllPosRe re "aaa"
            ,
            testCase "{2,} Lazy" $ do
                let re = Repeat 2 Nothing Lazy (atom 'a')
                assertEqual errorMsg [] $ findAllPosRe re ""
                assertEqual errorMsg [Matching 0 2, Matching 1 3] $ findAllPosRe re "aaa"
        ] ++
        
        map (\e -> testCase ("{1, 4}+Concat " ++ show e) $ do
            let re = Concat (atom 'b') $ Concat (Repeat 1 (Just 4) e (atom 'a')) (atom 'c')
            assertEqual errorMsg [Matching 1 6] $ findAllPosRe re "ebaaacd"
        ) [Lazy, Eager]
    ),
    testCase "Overlapping" $ do
        let re = Concat (Concat (atom 'a') (atom 'b')) (atom 'a')
        assertEqual errorMsg [Matching 0 3, Matching 2 5, Matching 4 7] $ findAllPosRe re "abababa"
    ,
    testCase "Nonoverlapping" $ do
        let re = Concat (Concat (atom 'a') (atom 'b')) (atom 'a')
        assertEqual errorMsg [Matching 0 3, Matching 4 7] $ findAllRe re "abababa"
    ,
    testCase "Lazy" $ do
        let re = Concat (atom 'a') (atom 'b')
        let res = take 3 . findAllRe re . concat $ repeat "cab"
        assertEqual errorMsg [Matching 1 3, Matching 4 6, Matching 7 9] res
        let res = take 3 . findAllPosRe re . concat $ repeat "cab"
        assertEqual errorMsg [Matching 1 3, Matching 4 6, Matching 7 9] res
    ]
