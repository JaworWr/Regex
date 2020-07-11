{-# LANGUAGE LambdaCase #-}

module Parser where

import DataTypes

import Control.Monad.State
import Control.Monad.Except
import Text.Printf
import Data.Char
import Data.Maybe

parse :: String -> Either ParseError Regex
parse = evalStateT (pRegex <* guardEOS) . stringToCursor

data ParseError = ParseError {
    errorPos :: Int,
    errorInfo :: ParseErrorInfo
} deriving (Eq, Show)

data ParseErrorInfo =
    UnexpectedEOS |
    ExpectedGot String (Maybe Char)
    deriving (Eq, Show)

prettyError :: ParseError -> String
prettyError (ParseError p i) = printf "Parse error at position %d: %s" p $ prettyErrorInfo i where
    prettyErrorInfo :: ParseErrorInfo -> String
    prettyErrorInfo UnexpectedEOS = "Unexpected end of input"
    prettyErrorInfo (ExpectedGot e g) = printf "Expected %s, got %s" (prettyExpected e) (prettyGot g)
    prettyExpected [] = "end of input"
    prettyExpected [c] = show c
    prettyExpected [c1, c2] = show c1 ++ " or " ++ show c2
    prettyExpected (c:cs) = show c ++ ", " ++ prettyExpected cs
    prettyGot = prettyExpected . maybeToList

-- Basic parser type and helper functions
type Parser = StateT Cursor (Either ParseError)

throwParseError :: Int -> ParseErrorInfo -> Parser a
throwParseError = (throwError .) . ParseError

throwParseErrorAtPos :: ParseErrorInfo -> Parser a
throwParseErrorAtPos i = do
    p <- pGetPos
    throwParseError p i

pPeekChar :: Parser (Maybe Char)
pPeekChar = gets peekChar

pGetPos :: Parser Int
pGetPos = gets curPos

fromJustWithEOF :: Maybe a -> Parser a
fromJustWithEOF = maybe (throwParseErrorAtPos UnexpectedEOS) return

pGetChar :: Parser Char
pGetChar = pPeekChar >>= fromJustWithEOF

pDropChar :: Parser ()
pDropChar = gets dropChar >>= fromJustWithEOF >>= put

pPopChar :: Parser Char
pPopChar = do
    (c, cur') <- gets popChar >>= fromJustWithEOF
    put cur'
    return c

guardEOS :: Parser ()
guardEOS = do
    c <- pPeekChar
    when (isJust c) . throwParseErrorAtPos $ ExpectedGot [] c

guardChar :: Char -> Parser ()
guardChar c = do
    c' <- pPeekChar
    when (c' /= Just c) . throwParseErrorAtPos $ ExpectedGot [c] c'

-- Atomic parsers
pAtomic :: Parser Regex
pAtomic = pPopChar >>= \case
    '\\' -> pEscaped
    '.' -> return . Atom $ AtomPredicate (const True) "wildcard"
    c -> return . Atom $ AtomPredicate (== c) [c]

pEscaped :: Parser Regex
pEscaped = pPopChar >>= \case
    'd' -> return . Atom $ AtomPredicate isDigit "digit"
    'D' -> return . Atom $ AtomPredicate (not . isDigit) "non-digit"
    's' -> return . Atom $ AtomPredicate isSpace "whitespace"
    'S' -> return . Atom $ AtomPredicate (not . isSpace) "non-whitespace"
    c -> return . Atom $ AtomPredicate (== c) [c]

-- Top level parser
pRegex :: Parser Regex
pRegex = undefined 
