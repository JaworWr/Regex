module DataTypes where

data AtomPredicate = AtomPredicate { atomPred :: Char -> Bool, atomPredDesc :: String }

instance Show AtomPredicate where
    show = atomPredDesc

data Eagerness = Eager | Lazy deriving (Eq, Show)

data Regex =
    Atom AtomPredicate |
    Concat Regex Regex |
    Or Regex Regex |
    Repeat Eagerness !Int !Int Regex
    deriving Show