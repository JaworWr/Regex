module Parser where

import DataTypes

parse :: String -> Either ParseError Regex
parse = undefined

data ParseError -- TODO

instance Show ParseError where
    show = undefined