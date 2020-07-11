module Matching where

import DataTypes

import Data.Maybe
import Data.List
import Data.Hashable
import qualified Data.HashSet as H

data Matching = Matching { matchingStart :: Int, matchingEnd :: Int } deriving (Eq, Show)

instance Hashable Matching where
    hashWithSalt salt (Matching s e) = hashWithSalt salt (s, e)

nubHashable :: (Eq a, Hashable a) => [a] -> [a]
nubHashable = aux H.empty where
    aux _ [] = []
    aux s (x:xs) 
        | x `H.member` s = aux s xs
        | otherwise = x : aux (H.insert x s) xs

matchingLen :: Matching -> Int
matchingLen m = matchingEnd m - matchingStart m

findAllRe :: Regex -> String -> [Matching]
findAllRe re s = nubHashable $ aux (stringToCursor s) where
    sc cur cur' fc' = maybe (m:fc') (((m:fc')++) . aux) $ dropChar cur where
        m = Matching (curPos cur) (curPos cur')
    fc cur = maybe [] aux $ dropChar cur
    aux cur = match re cur (sc cur) (fc cur)

findNonoverlappingRe :: Regex -> String -> [Matching]
findNonoverlappingRe re s = aux (stringToCursor s) where
    sc cur cur' fc'
        | eos cur' = [m]
        | otherwise = m:aux cur'
        where
            m = Matching (curPos cur) (curPos cur')
    fc cur = maybe [] aux $ dropChar cur
    aux cur = match re cur (sc cur) (fc cur)

searchRe :: Regex -> String -> Maybe Matching
searchRe = (listToMaybe .) . findAllRe

-- Regex matching
type Fc a = a
type Sc a = Cursor -> Fc a -> a

decr :: Num a => Maybe a -> Maybe a
decr = fmap $ subtract 1

match :: Regex -> Cursor -> Sc a -> Fc a -> a
match (Atom ap) cur sc fc = case popChar cur of
    Just (c, cur') | atomPred ap c -> sc cur' fc
    _ -> fc
match (Concat re1 re2) cur sc fc = 
    match re1 cur (\cur' -> match re2 cur' sc) fc
match (Or re1 re2) cur sc fc =
    match re1 cur sc (match re2 cur sc fc)
match (Repeat 0 (Just 0) _ _) cur sc fc = sc cur fc
match (Repeat _ (Just 0) _ _) _ _ fc = fc
match (Repeat 0 m Eager re) cur sc fc = match re cur cont $ sc cur fc where
    cont cur' = match (Repeat 0 (decr m) Eager re) cur' sc
match (Repeat 0 m Lazy re) cur sc fc = sc cur $ match re cur cont fc where
    cont cur' = match (Repeat 0 (decr m) Lazy re) cur' sc
match (Repeat n m e re) cur sc fc = match re cur cont fc where
    cont cur' = match (Repeat (n-1) (decr m) e re) cur' sc
match BOS cur sc fc
    | curPos cur == 0 = sc cur fc
    | otherwise = fc
match EOS cur sc fc
    | eos cur = sc cur fc
    | otherwise = fc
