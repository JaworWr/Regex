module DataTypes where

import Data.Maybe
import Data.List
import Atom

-- Regex representation
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
