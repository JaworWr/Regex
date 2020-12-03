{-# LANGUAGE StandaloneDeriving #-}

module ParserTest (tests) where

import Control.Monad
import Control.Monad.State

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import DataTypes
import Parser

atomPredErr desc i = "Wrong predicate result for atom " ++ desc ++ ", input " ++ show i
parseResultError i res = "Wrong parse result for input " ++ show i ++ ": " ++ show res

instance Eq AtomPredicate where
    (AtomPredicate _ d1) == (AtomPredicate _ d2) = d1 == d2
deriving instance Eq Regex

mockAtom :: String -> Regex
mockAtom desc = Atom (AtomPredicate undefined desc)

testGetAtom :: String -> String -> IO (Char -> Bool)
testGetAtom desc s = case parse s of
    Right (Atom (AtomPredicate pr s1)) | s1 == desc -> return pr
    res -> assertFailure $ "Wrong parse result for atom " ++ desc ++ ": " ++ show res

testRepeat :: Int -> Maybe Int -> Eagerness -> String -> IO ()
testRepeat n m e s = case parse s of
    Right (Repeat n1 m1 e1 _)
        | n1 == n && m1 == m && e1 == e -> return ()
    res -> assertFailure $ parseResultError s res

testParseError :: Int -> ParseErrorInfo -> String -> IO ()
testParseError pos info s = case parse s of
    Left (ParseError pos1 info1)
        | pos == pos1 && info == info1 -> return ()
    res -> assertFailure $ parseResultError s res

testParser :: (Eq a, Show a) => Parser a -> a -> String -> IO ()
testParser p x s = case evalStateT p $ stringToCursor s of
    Right x1 | x1 == x -> return ()
    res -> assertFailure $ parseResultError s res

testRegex :: Regex -> String -> IO ()
testRegex = testParser pRegex

tests = testGroup "Parser" [
    testCase "Atom" $ do 
        pr <- testGetAtom "'a'" "a"
        assertBool (atomPredErr "'a'" 'a') $ pr 'a'
        assertBool (atomPredErr "'a'" 'b') $ not $ pr 'b'

        void $ testGetAtom "'.'" "\\."
        void $ testGetAtom "wildcard" "."
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
            pr <- testGetAtom desc s
            zipWithM_ (singlePredTest desc pr) ['1', ' ', 'a'] res
    in map runTest testData
    ,
    testCase "AtomModifiers" $ do
        testRepeat 0 Nothing Eager "a*"
        testRepeat 1 Nothing Eager "a+"
        testRepeat 0 (Just 1) Eager "a?"
        testRepeat 0 Nothing Lazy "a*?"
        testRepeat 1 Nothing Lazy "a+?"
        testRepeat 0 (Just 1) Lazy "a??"
        testParseError 2 MultipleRepeats "a*+"
    ,
    testCase "SpecialAtoms" $ do
        testRegex BOS "^"
        testRegex EOS "$"
    ,
    testCase "Concat" $
    let re = Concat 
            (Concat (mockAtom "'a'") (mockAtom "'b'"))
            (mockAtom "'c'")
    in testRegex re "abc"
    ,
    testCase "Or" $
    let re = Or 
            (Concat (mockAtom "'a'") (mockAtom "'b'"))
            (mockAtom "'c'")
    in testRegex re "ab|c"
    ,
    testCase "Parentheses" $
    let re = Concat 
            (mockAtom "'a'")
            (Or (mockAtom "'b'") (mockAtom "'c'"))
    in testRegex re "a(b|c)"
    ,
    testCase "Integer" $ do
        testParser (fromOptional pInteger) (Just 123) "123"
        testParser (fromOptional pInteger) (Just 123) "0123"
        testParser (fromOptional pInteger) (Just 123) "123a"
        testParser (fromOptional pInteger) Nothing "a123"
    ,
    testCase "AtomRepeats" $ do
        testRepeat 5 (Just 5) Eager "a{5}"
        testRepeat 5 (Just 5) Lazy "a{5}?"
        testRepeat 5 (Just 7) Eager "a{5,7}"
        testRepeat 5 (Just 7) Lazy "a{5,7}?"
        testRepeat 5 Nothing Eager "a{5,}"
        testRepeat 5 Nothing Lazy "a{5,}?"
        testRepeat 0 (Just 7) Eager "a{,7}"
        testRepeat 0 (Just 7) Lazy "a{,7}?"
        testParseError 3 (ExpectedGot "}" Nothing) "a{5"
        testParseError 3 (ExpectedGot "}" (Just 'c')) "a{5c}"
    ,
    testGroup "Concat + Modifiers" [
        testCase "Star" $
        let re = Concat (Repeat 0 Nothing Eager (mockAtom "'a'")) (mockAtom "'b'")
        in testRegex re "a*b"
        ,
        testCase "Repeats" $
        let re = Concat (Repeat 3 (Just 5) Eager (mockAtom "'a'")) (mockAtom "'b'")
        in testRegex re "a{3,5}b"
        ]
    ,
    testCase "Character group" $ do
        testRegex (mockAtom "'-','a','-','5-9',digit,'u','[','-'") "[-a\\-5-9\\du[-]"
        testRegex (mockAtom "NOT['-','a','-','5-9',digit,'u','[','-']") "[^-a\\-5-9\\du[-]"
        testRegex (mockAtom "'^'") "[\\^]"
        testRegex (mockAtom "NOT['^']") "[^^]"
    ,
    testCase "Concat + Character group" $
    let re = Concat (Concat (mockAtom "'a-e'") (mockAtom "'f','g'")) (mockAtom "'h'")
    in testRegex re "[a-e][fg]h" 
    ]
