{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Parser where

import DataTypes

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Maybe
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
    ExpectedGot String (Maybe Char) |
    MultipleRepeats |
    IncorrectRange
    deriving (Eq, Show)

prettyError :: ParseError -> String
prettyError (ParseError p i) = printf "Parse error at position %d: %s" p $ prettyErrorInfo i where
    prettyErrorInfo :: ParseErrorInfo -> String
    prettyErrorInfo (Unexpected g) = "Unexpected " ++ prettyGot g
    prettyErrorInfo (ExpectedGot e g) = printf "Expected %s, got %s" (prettyExpected e) (prettyGot g)
    prettyErrorInfo MultipleRepeats = "Multiple repeat modifiers"
    prettyExpected [] = "end of input"
    prettyExpected [c] = show c
    prettyExpected [c1, c2] = show c1 ++ " or " ++ show c2
    prettyExpected (c:cs) = show c ++ ", " ++ prettyExpected cs
    prettyGot = prettyExpected . maybeToList

-- Basic parser types and helper functions
type Parser = StateT Cursor (Either ParseError)
type OptionalParser = MaybeT Parser

class (Monad m, MonadState Cursor m, MonadError ParseError m) => MonadParser (m :: * -> *)
instance (Monad m, MonadState Cursor m, MonadError ParseError m) => MonadParser m

fromOptional :: OptionalParser a -> Parser (Maybe a)
fromOptional = runMaybeT

throwParseError :: MonadParser m => Int -> ParseErrorInfo -> m a
throwParseError = (throwError .) . ParseError

throwParseErrorAtPos :: MonadParser m => ParseErrorInfo -> m a
throwParseErrorAtPos i = do
    p <- pGetPos
    throwParseError p i

pPeekChar :: MonadParser m => m (Maybe Char)
pPeekChar = gets peekChar

pGetPos :: MonadParser m => m Int
pGetPos = gets curPos

fromJustWithEOF :: MonadParser m => Maybe a -> m a
fromJustWithEOF = maybe (throwParseErrorAtPos $ Unexpected Nothing) return

pGetChar :: MonadParser m => m Char
pGetChar = pPeekChar >>= fromJustWithEOF

pDropChar :: MonadParser m =>  m ()
pDropChar = gets dropChar >>= fromJustWithEOF >>= put

pPopChar :: MonadParser m => m Char
pPopChar = do
    (c, cur') <- gets popChar >>= fromJustWithEOF
    put cur'
    return c

guardEOS :: MonadParser m => m ()
guardEOS = do
    c <- pPeekChar
    when (isJust c) . throwParseErrorAtPos $ ExpectedGot [] c

guardChar :: MonadParser m => Char -> m ()
guardChar c = do
    c' <- pPeekChar
    when (c' /= Just c) . throwParseErrorAtPos $ ExpectedGot [c] c'

-- Atomic parsers
pAtomic :: Parser Regex
pAtomic = pPopChar >>= \case
    '\\' -> pEscaped
    '.' -> return . Atom $ AtomPredicate (const True) "wildcard"
    '(' -> pRegex <* guardChar ')' <* pDropChar
    '[' -> do
        postpr <- pGetChar >>= \case
            '^' -> pDropChar >> return negatePredicate
            _ -> return id
        pCharGroup postpr <* guardChar ']' <* pDropChar
    '^' -> return BOS
    '$' -> return EOS
    c -> return . Atom $ charPredicate c

pEscaped :: Parser Regex
pEscaped = do
    c <- pPopChar
    return . Atom . fromMaybe (charPredicate c) $ escapedPredicate c

escapedPredicate :: Char -> Maybe AtomPredicate
escapedPredicate 'd' = return $ AtomPredicate isDigit "digit"
escapedPredicate 'D' = return $ AtomPredicate (not . isDigit) "non-digit"
escapedPredicate 's' = return $ AtomPredicate isSpace "whitespace"
escapedPredicate 'S' = return $ AtomPredicate (not . isSpace) "non-whitespace"
escapedPredicate 'w' = return $ AtomPredicate isAlpha "word"
escapedPredicate 'W' = return $ AtomPredicate (not . isAlpha) "non-word"
escapedPredicate _ = Nothing

-- Atom modifier parsers
pInteger :: OptionalParser Int
pInteger = pPeekChar >>= \case
    Just c | isDigit c -> pDropChar >> pIntTail (digitToInt c)
    _ -> mzero
    where
        pIntTail !x = pPeekChar >>= \case
            Just c | isDigit c -> pDropChar >> pIntTail (10 * x + digitToInt c)
            _ -> return x

pEagerness :: OptionalParser Eagerness
pEagerness = pPeekChar >>= \case
    Just '?' -> pDropChar >> return Lazy
    _ -> return Eager

pRepeats :: Parser (Eagerness -> Regex -> Regex)
pRepeats = do
    x <- fromMaybe 0 <$> fromOptional pInteger
    pPeekChar >>= \case
        Just ',' -> do
            pDropChar
            y <- fromOptional pInteger
            return $ Repeat x y
        _ -> return $ Repeat x (Just x)

pModifier :: Parser (Maybe (Regex -> Regex))
pModifier = pPeekChar >>= fromOptional . \case
    Just '*' -> pDropChar >> Repeat 0 Nothing <$> pEagerness
    Just '+' -> pDropChar >> Repeat 1 Nothing <$> pEagerness
    Just '?' -> pDropChar >> Repeat 0 (Just 1) <$> pEagerness
    Just '{' -> pDropChar >> (lift pRepeats <* guardChar '}' <* pDropChar) <*> pEagerness
    _ -> mzero

pAtomicWithModifier :: Parser Regex
pAtomicWithModifier = do
    a <- pAtomic
    m <- pModifier
    -- check if we have only one repeat modifier
    repPos <- pGetPos
    rep <- isJust <$> pModifier
    when rep $ throwParseError repPos MultipleRepeats
    return $ fromMaybe id m a

-- Character group parser
pCharGroup :: (AtomPredicate -> AtomPredicate) -> Parser Regex
pCharGroup postpr = Atom . postpr . mconcat <$> pCharGroupElems where
    pCharGroupElems = fromOptional pCharGroupElem >>= \case
        Just c -> (c:) <$> pCharGroupElems
        Nothing -> return []

pCharGroupElem :: OptionalParser AtomPredicate
pCharGroupElem = pGetChar >>= \case
    ']' -> mzero
    '\\' -> do
        pos <- pGetPos
        pDropChar
        c <- pPopChar
        maybe (pCharGroupRange pos c) return $ escapedPredicate c
    c -> do
        pos <- pGetPos
        pDropChar
        pCharGroupRange pos c

pCharGroupRange :: Int -> Char -> OptionalParser AtomPredicate
pCharGroupRange pos c = pGetChar >>= \case
    '-' -> pDropChar >> pGetChar >>= \case
        ']' -> return $ charPredicate c <> charPredicate '-'
        '\\' -> do
            pDropChar
            c1 <- pPopChar
            case escapedPredicate c1 of
                Nothing -> maybe (throwParseError pos IncorrectRange) return (rangePredicate c c1)
                Just pr -> return $ charPredicate c <> charPredicate '-' <> pr
        c1 -> pDropChar >> 
            maybe (throwParseError pos IncorrectRange) return (rangePredicate c c1)
    _ -> return $ charPredicate c

-- Intermediate parsers
pConcat :: Parser Regex
pConcat = pAtomicWithModifier >>= pConcatTail where
    pConcatTail re = pPeekChar >>= \case
        Just c | c `notElem` ignored -> pAtomicWithModifier >>= pConcatTail . Concat re
        _ -> return re
    ignored = ")|"

pOr :: Parser Regex
pOr = pConcat >>= pOrTail where
    pOrTail re = pPeekChar >>= \case
        Just '|' -> pDropChar >> Or re <$> pConcat >>= pOrTail
        _ -> return re

-- Top level parser
pRegex :: Parser Regex
pRegex = pOr 
