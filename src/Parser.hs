module Parser where

import DataTypes

import Control.Monad.State
import Control.Monad.Except
import Text.Printf

parse :: String -> Either ParseError Regex
parse = undefined

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


