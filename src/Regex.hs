module Regex (
    Regex(),
    ParseError(),
    parse,
    prettyError,
    Matching(),
    matchingStart, 
    matchingEnd, 
    matchingLen,
    findAllRe,
    findAllPosRe,
    searchRe,
    findAll,
    findAllPos,
    search
) where

import DataTypes
import Matching
import Parser

findAll = parseAndThen findAllRe
findAllPos = parseAndThen findAllPosRe
search = parseAndThen searchRe

parseAndThen :: (Regex -> String -> a) -> String -> String -> Either ParseError a
parseAndThen f rs s = f <$> parse rs <*> return s
