module DataTypes where

-- Regex representation
data AtomPredicate = AtomPredicate { atomPred :: Char -> Bool, atomPredDesc :: String }

instance Show AtomPredicate where
    show = atomPredDesc

data Eagerness = Eager | Lazy deriving (Eq, Show)

data Regex =
    Atom AtomPredicate |
    Concat Regex Regex |
    Or Regex Regex |
    Repeat Eagerness !Int !Int Regex |
    BOS |
    EOS
    deriving Show
