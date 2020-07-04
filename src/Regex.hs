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
    searchRe,
    findAll,
    findNonoverlapping,
    search
) where

import DataTypes
import Matching
import Parser

findAll = parseAndThen findAllRe
findNonoverlapping = parseAndThen findNonoverlappingRe
search = parseAndThen searchRe

parseAndThen :: (Regex -> String -> a) -> String -> String -> Either ParseError a
parseAndThen f rs s = f <$> parse rs <*> return s
