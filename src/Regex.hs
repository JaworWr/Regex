module Regex (
    Regex(),
    ParseError(),
    parse,
    Matching(),
    matchingStart, 
    matchingEnd, 
    matchingLen,
    findAllRe,
    findNonoverlappingRe,
    matchRe,
    searchRe,
    findAll,
    findNonoverlapping,
    match,
    search
) where

import DataTypes
import Matching
import Parser

findAll = parseAndThen findAllRe
findNonoverlapping = parseAndThen findNonoverlappingRe
match = parseAndThen matchRe
search = parseAndThen searchRe

parseAndThen :: (Regex -> String -> a) -> String -> String -> Either ParseError a
parseAndThen f rs s = f <$> parse rs <*> return s
