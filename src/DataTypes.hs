module DataTypes where

import Data.Maybe
import Data.List

-- Regex representation
data AtomPredicate = AtomPredicate { atomPred :: Char -> Bool, atomPredDesc :: String }

instance Show AtomPredicate where
    show = atomPredDesc

instance Semigroup AtomPredicate where
    (AtomPredicate pr1 d1) <> (AtomPredicate pr2 d2) =
        let combinedDesc
                | null d1 = d2
                | null d2 = d1
                | otherwise = d1 ++ "," ++ d2
        in AtomPredicate (\a -> pr1 a || pr2 a) combinedDesc

instance Monoid AtomPredicate where
    mempty = AtomPredicate (const False) ""

charPredicate :: Char -> AtomPredicate
charPredicate c = AtomPredicate (== c) $ show c

rangePredicate :: Char -> Char -> Maybe AtomPredicate
rangePredicate c1 c2 
    | c1 <= c2 = Just . AtomPredicate (\a -> c1 <= a && a <= c2) $ 
        "'" ++ [c1] ++ "-" ++ [c2] ++ "'"
    | otherwise = Nothing

data Eagerness = Eager | Lazy deriving (Eq, Show)

data Regex =
    Atom AtomPredicate |
    Concat Regex Regex |
    Or Regex Regex |
    Repeat !Int !(Maybe Int) Eagerness Regex |
    BOS |
    EOS
    deriving Show

-- String cursor and helper functions
data Cursor = Cursor { curPos :: !Int, curData :: String } deriving (Eq, Show)

eos :: Cursor -> Bool
eos = null . curData

popChar :: Cursor -> Maybe (Char, Cursor)
popChar cur = fmap newCursor . uncons $ curData cur where
    newCursor (c, s) = (c, Cursor (curPos cur + 1) s)

peekChar :: Cursor -> Maybe Char
peekChar = listToMaybe . curData

dropChar :: Cursor -> Maybe Cursor
dropChar = fmap snd . popChar

stringToCursor :: String -> Cursor
stringToCursor = Cursor 0
