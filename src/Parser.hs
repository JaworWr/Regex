{-# LANGUAGE LambdaCase #-}

module Parser where

import DataTypes

import Control.Monad.State
import Control.Monad.Except
import Text.Printf
import Data.Char

parse :: String -> Either ParseError Regex
parse = evalStateT (pRegex <* guardEOF) . stringToCursor

data ParseError = ParseError {
    errorPos :: Int,
    errorDesc :: String
}

instance Show ParseError where
    show (ParseError p d) = printf "Parse error at position %d: %s" p d

curError :: String -> Cursor -> ParseError
curError d cur = ParseError (curPos cur) d

-- Basic parser type and helper functions
type Parser = StateT Cursor (Either ParseError)

pPeekChar :: Parser (Maybe Char)
pPeekChar = gets peekChar

pGetPos :: Parser Int
pGetPos = gets curPos

fromJustWithEOF :: Maybe a -> Parser a
fromJustWithEOF m = do
    p <- pGetPos
    maybe (throwError $ ParseError p "Unexpected end of input") return m

pGetChar :: Parser Char
pGetChar = pPeekChar >>= fromJustWithEOF

pDropChar :: Parser ()
pDropChar = gets dropChar >>= fromJustWithEOF >>= put

pPopChar :: Parser Char
pPopChar = do
    (c, cur') <- gets popChar >>= fromJustWithEOF
    put cur'
    return c

guardEOF :: Parser ()
guardEOF = pPeekChar >>= \case
    Just c -> do
        p <- pGetPos
        throwError . ParseError p $ printf "Unexpected '%c', expected end of input" c
    Nothing -> return ()

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
