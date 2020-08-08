module ParserTest (tests) where

import Control.Monad
import Control.Monad.State

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import DataTypes
import Parser

testAtom desc res = case res of
    Right (Atom (AtomPredicate pr s)) | s == desc -> return pr
    _ -> assertFailure $ "Wrong parse result for atom " ++ desc ++ ": " ++ show res

atomPredErr desc i = "Wrong predicate result for atom " ++ desc ++ ", input " ++ show i
parseResultError i res = "Wrong parse result for input " ++ show i ++ ": " ++ show res

tests = testGroup "Parser" [
    testCase "Atom" $ do 
        pr <- testAtom "'a'" (parse "a")
        assertBool (atomPredErr "'a'" 'a') $ pr 'a'
        assertBool (atomPredErr "'a'" 'b') $ not $ pr 'b'

        void $ testAtom "'.'" (parse "\\.")
        void $ testAtom "wildcard" (parse ".")
    ,
    testGroup "CharTypes" $ 
        let testData = [
                ("\\d", [True, False, False], "digit"),
                ("\\D", [False, True, True], "non-digit"),
                ("\\s", [False, True, False], "whitespace"),
                ("\\S", [True, False, True], "non-whitespace"),
                ("\\w", [False, False, True], "word"),
                ("\\W", [True, True, False], "non-word")
                ]
            singlePredTest desc pr i =
                assertEqual (atomPredErr desc i) (pr i)
            runTest (s, res, desc) = testCase desc $ do
                pr <- testAtom desc (parse s)
                zipWithM_ (singlePredTest desc pr) ['1', ' ', 'a'] res
            in map runTest testData
    ,
    testCase "AtomModifiers" $ do
        case parse "a*" of
            Right (Repeat 0 Nothing Eager _) -> return ()
            res -> assertFailure $ parseResultError "a*" res
        case parse "a+" of
            Right (Repeat 1 Nothing Eager _) -> return ()
            res -> assertFailure $ parseResultError "a+" res
        case parse "a?" of
            Right (Repeat 0 (Just 1) Eager _) -> return ()
            res -> assertFailure $ parseResultError "a?" res
        case parse "a*?" of
            Right (Repeat 0 Nothing Lazy _) -> return ()
            res -> assertFailure $ parseResultError "a*?" res
        case parse "a+?" of
            Right (Repeat 1 Nothing Lazy _) -> return ()
            res -> assertFailure $ parseResultError "a+?" res
        case parse "a??" of
            Right (Repeat 0 (Just 1) Lazy _) -> return ()
            res -> assertFailure $ parseResultError "a??" res
        case parse "a*+" of
            Left (ParseError 2 MultipleRepeats) -> return ()
            res -> assertFailure $ parseResultError "a*" res
    ,
    testCase "Concat" $ case parse "abc" of
            Right (Concat 
                    (Concat (Atom (AtomPredicate _ "'a'")) (Atom (AtomPredicate _ "'b'")))
                    (Atom (AtomPredicate _ "'c'"))
                ) -> return ()
            res -> assertFailure $ parseResultError "abc" res
    ,
    testCase "Or" $ case parse "ab|c" of
        Right (Or 
                (Concat (Atom (AtomPredicate _ "'a'")) (Atom (AtomPredicate _ "'b'")))
                (Atom (AtomPredicate _ "'c'"))
            ) -> return ()
        res -> assertFailure $ parseResultError "ab|c" res
    ,
    testCase "Parentheses" $ case parse "a(b|c)" of
        Right (Concat 
                (Atom (AtomPredicate _ "'a'"))
                (Or (Atom (AtomPredicate _ "'b'")) (Atom (AtomPredicate _ "'c'")))
            ) -> return ()
        res -> assertFailure $ parseResultError "a(b|c)" res
    ,
    testCase "Integer" $ do
        assertEqual "Wrong parse result"
            (evalStateT (fromOptional pInteger) $ stringToCursor "123")
            (Right $ Just 123)
        assertEqual "Wrong parse result"
            (evalStateT (fromOptional pInteger) $ stringToCursor "0123")
            (Right $ Just 123)
        assertEqual "Wrong parse result"
            (evalStateT (fromOptional pInteger) $ stringToCursor "123a")
            (Right $ Just 123)
        assertEqual "Wrong parse result"
            (evalStateT (fromOptional pInteger) $ stringToCursor "a123")
            (Right Nothing)
    ,
    testCase "AtomRepeats" $ do
        case parse "a{5}" of
            Right (Repeat 5 (Just 5) Eager _) -> return ()
            res -> assertFailure $ parseResultError "a{5}" res
        case parse "a{5}?" of
            Right (Repeat 5 (Just 5) Lazy _) -> return ()
            res -> assertFailure $ parseResultError "a{5}" res
        case parse "a{5,7}" of
            Right (Repeat 5 (Just 7) Eager _) -> return ()
            res -> assertFailure $ parseResultError "a{5,7}" res
        case parse "a{5,7}?" of
            Right (Repeat 5 (Just 7) Lazy _) -> return ()
            res -> assertFailure $ parseResultError "a{5,7}?" res
        case parse "a{5,}" of
            Right (Repeat 5 Nothing Eager _) -> return ()
            res -> assertFailure $ parseResultError "a{5,}" res
        case parse "a{5,}?" of
            Right (Repeat 5 Nothing Lazy _) -> return ()
            res -> assertFailure $ parseResultError "a{5,}?" res
        case parse "a{,7}" of
            Right (Repeat 0 (Just 7) Eager _) -> return ()
            res -> assertFailure $ parseResultError "a{,7}" res
        case parse "a{,7}?" of
            Right (Repeat 0 (Just 7) Lazy _) -> return ()
            res -> assertFailure $ parseResultError "a{,7}?" res
        case parse "a{5" of
            Left _ -> return ()
            res -> assertFailure $ parseResultError "a{5" res
        case parse "a{5c}" of
            Left _ -> return ()
            res -> assertFailure $ parseResultError "a{5c}" res
    ,
    testGroup "Concat + Modifiers" [
        testCase "Star" $ case parse "a*b" of
            Right (Concat (Repeat 0 Nothing Eager _) (Atom _)) -> return ()
            res -> assertFailure $ parseResultError "a*b" res
        ,
        testCase "Repeats" $ case parse "a{3,5}b" of
            Right (Concat (Repeat 3 (Just 5) Eager _) (Atom _)) -> return ()
            res -> assertFailure $ parseResultError "a{3,5}b" res
        ]
    ,
    testCase "Character group" $ case parse "[-a\\-5-9\\du-]" of
        Right (Atom (AtomPredicate _ "'-','a','-','5-9',digit,'u','-'")) -> return ()
        res -> assertFailure $ parseResultError "[a\\-5-9\\du-]" res
    ]
