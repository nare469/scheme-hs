module Parser where

import Text.ParserCombinators.Parsec

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

ignoreSpaces :: Parser ()
ignoreSpaces = skipMany space

readExpr :: String -> String
readExpr input = case parse (ignoreSpaces >> symbol) "Symbol" input of
    Left err -> "No match: " ++ show err
    Right val -> "It's a match!"
