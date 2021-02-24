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
    deriving (Eq, Show)

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

ignoreCaseRegex :: Regex -> Regex
ignoreCaseRegex (Atom pr) = Atom (ignoreCase pr)
ignoreCaseRegex (Concat re1 re2) = Concat (ignoreCaseRegex re1) (ignoreCaseRegex re2)
ignoreCaseRegex (Or re1 re2) = Or (ignoreCaseRegex re1) (ignoreCaseRegex re2)
ignoreCaseRegex (Repeat n m e re) = Repeat n m e (ignoreCaseRegex re)
ignoreCaseRegex re = re
