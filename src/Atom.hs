module Atom where

import Data.Char

-- atom predicate representation
data AtomPredicate =
    AtomChar Char |
    AtomRange Char Char |
    AtomWildcard |
    AtomNull |
    AtomNot AtomPredicate |
    AtomOr AtomPredicate AtomPredicate |
    AtomDigit |
    AtomSpace |
    AtomAlpha |
    AtomIChar Char |
    AtomIRange Char Char
    deriving Eq


makeRange :: Char -> Char -> Maybe AtomPredicate
makeRange c1 c2
    | c1 <= c2 = Just $ AtomRange c1 c2
    | otherwise = Nothing


instance Show AtomPredicate where
    show (AtomChar c) = ['\'', c, '\'']
    show (AtomRange c1 c2) = ['\'', c1, '-', c2, '\'']
    show AtomWildcard = "<wildcard>"
    show AtomNull = "<null>"
    show (AtomNot pr) = "NOT[" ++ show pr ++ "]"
    show (AtomOr pr1 pr2) = show pr1 ++ "," ++ show pr2
    show AtomDigit = "<digit>"
    show AtomSpace = "<whitespace>"
    show AtomAlpha = "<alphanumeric>"
    show (AtomIChar c) = show (AtomChar c)
    show (AtomIRange c1 c2) = show (AtomRange c1 c2)


instance Semigroup AtomPredicate where
    AtomNull <> pr1 = pr1
    pr2 <> AtomNull = pr2
    pr1 <> pr2 = AtomOr pr1 pr2

instance Monoid AtomPredicate where
    mempty = AtomNull


getPredicate :: AtomPredicate -> Char -> Bool
getPredicate (AtomChar c) = (== c)
getPredicate (AtomRange c1 c2) = \a -> c1 <= a && a <= c2 
getPredicate AtomWildcard = const True 
getPredicate AtomNull = const False
getPredicate (AtomNot pr) = not . getPredicate pr
getPredicate (AtomOr pr1 pr2) = (||) <$> getPredicate pr1 <*> getPredicate pr2
getPredicate AtomDigit = isDigit
getPredicate AtomSpace = isSpace
getPredicate AtomAlpha = isAlpha
getPredicate (AtomIChar c) = (== c) . toLower
getPredicate (AtomIRange c1 c2) = (\a -> c1 <= a && a <= c2) . toLower


ignoreCase :: AtomPredicate -> AtomPredicate
ignoreCase (AtomChar c) = AtomIChar (toLower c)
ignoreCase (AtomRange c1 c2) = AtomIRange (toLower c1) (toLower c2)
ignoreCase (AtomNot pr) = AtomNot (ignoreCase pr)
ignoreCase (AtomOr pr1 pr2) = AtomOr (ignoreCase pr1) (ignoreCase pr2)
ignoreCase pr = pr
