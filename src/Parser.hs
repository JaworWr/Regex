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
    Unexpected (Maybe Char) |
    ExpectedGot String (Maybe Char)
    deriving (Eq, Show)

prettyError :: ParseError -> String
prettyError (ParseError p i) = printf "Parse error at position %d: %s" p $ prettyErrorInfo i where
    prettyErrorInfo :: ParseErrorInfo -> String
    prettyErrorInfo (Unexpected g) = "Unexpected " ++ prettyGot g
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
fromJustWithEOF = maybe (throwParseErrorAtPos $ Unexpected Nothing) return

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
    '(' -> pRegex <* guardChar ')' <* pDropChar
    c -> return . Atom $ AtomPredicate (== c) $ show c

escapedPredicate :: Char -> Maybe AtomPredicate
escapedPredicate 'd' = return $ AtomPredicate isDigit "digit"
escapedPredicate 'D' = return $ AtomPredicate (not . isDigit) "non-digit"
escapedPredicate 's' = return $ AtomPredicate isSpace "whitespace"
escapedPredicate 'w' = return $ AtomPredicate isAlpha "word"
escapedPredicate 'S' = return $ AtomPredicate (not . isSpace) "non-whitespace"
escapedPredicate 'W' = return $ AtomPredicate (not . isAlpha) "non-word"
escapedPredicate _ = Nothing

pEscaped :: Parser Regex
pEscaped = do
    c <- pPopChar
    maybe 
        (return . Atom $ AtomPredicate (== c) $ show c)
        (return . Atom)
        (escapedPredicate c)

-- Top level parser
pRegex :: Parser Regex
pRegex = undefined 
