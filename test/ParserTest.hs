module ParserTest (tests) where

import Control.Monad
import Control.Monad.State

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import DataTypes
import Parser
import Atom

atomPredErr desc i = "Wrong predicate result for atom " ++ desc ++ ", input " ++ show i
parseResultError i res = "Wrong parse result for input " ++ show i ++ ": " ++ show res

charPredicate :: Char -> Regex
charPredicate = Atom . AtomChar

testGetAtom :: AtomPredicate ->  String -> IO (Char -> Bool)
testGetAtom pr1 s = case parse s of
    Right (Atom pr) | pr1 == pr -> return (getPredicate pr)
    res -> assertFailure $ "Wrong parse result for atom " ++ s ++ ": " ++ show res

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
        pr <- testGetAtom (AtomChar 'a') "a"
        assertEqual "Incorrect description" (show $ AtomChar 'a') "'a'"
        assertBool (atomPredErr "'a'" 'a') $ pr 'a'
        assertBool (atomPredErr "'a'" 'b') $ not $ pr 'b'

        void $ testGetAtom (AtomChar '.')  "\\."
        void $ testGetAtom AtomWildcard  "."
    ,
    testGroup "CharTypes" $ 
    let testData = [
            ("\\d", [True, False, False], AtomDigit, "<digit>"),
            ("\\D", [False, True, True], AtomNot AtomDigit, "NOT[<digit>]"),
            ("\\s", [False, True, False], AtomSpace, "<whitespace>"),
            ("\\S", [True, False, True], AtomNot AtomSpace, "NOT[<whitespace>]"),
            ("\\w", [False, False, True], AtomAlpha, "<alphanumeric>"),
            ("\\W", [True, True, False], AtomNot AtomAlpha, "NOT[<alphanumeric>]")
            ]
        singlePredTest desc pr i =
            assertEqual (atomPredErr desc i) (pr i)
        runTest (s, res, t, desc) = testCase desc $ do
            assertEqual "Incorrect description" (show t) desc
            pr <- testGetAtom t s
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
            (Concat (charPredicate 'a') (charPredicate 'b'))
            (charPredicate 'c')
    in testRegex re "abc"
    ,
    testCase "Or" $
    let re = Or 
            (Concat (charPredicate 'a') (charPredicate 'b'))
            (charPredicate 'c')
    in testRegex re "ab|c"
    ,
    testCase "Parentheses" $
    let re = Concat 
            (charPredicate 'a')
            (Or (charPredicate 'b') (charPredicate 'c'))
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
        let re = Concat (Repeat 0 Nothing Eager (charPredicate 'a')) (charPredicate 'b')
        in testRegex re "a*b"
        ,
        testCase "Repeats" $
        let re = Concat (Repeat 3 (Just 5) Eager (charPredicate 'a')) (charPredicate 'b')
        in testRegex re "a{3,5}b"
        ]
    ,
    testCase "Character group" $ do
        let re = mconcat [AtomChar '-', AtomChar 'a', AtomChar '-', AtomRange '5' '9', AtomDigit , AtomChar 'u', AtomChar '[', AtomChar '-']
        testRegex (Atom re) "[-a\\-5-9\\du[-]"
        testRegex (Atom $ AtomNot re) "[^-a\\-5-9\\du[-]"
        testRegex (charPredicate '^') "[\\^]"
        testRegex (Atom $ AtomNot (AtomChar '^')) "[^^]"
    ,
    testCase "Concat + Character group" $
    let re = Concat (Concat (Atom $ AtomRange 'a' 'e') (Atom $ AtomChar 'f' <> AtomChar 'g')) (charPredicate 'h')
    in testRegex re "[a-e][fg]h" 
    ]
