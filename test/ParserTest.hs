module ParserTest (tests) where

import Control.Monad

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
    ]
