module Matching where

import DataTypes

import Data.Maybe

data Matching = Matching { matchingStart :: Int, matchingEnd :: Int } deriving (Eq, Show)

matchingLen :: Matching -> Int
matchingLen m = matchingEnd m - matchingStart m

findAllRe :: Regex -> String -> [Matching]
findAllRe = undefined

findNonoverlappingRe :: Regex -> String -> [Matching]
findNonoverlappingRe = undefined

matchRe :: Regex -> String -> Maybe Matching
matchRe = undefined 

searchRe :: Regex -> String -> Maybe Matching
searchRe = (listToMaybe .) . findAllRe
