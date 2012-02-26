module Parser where

import Language
import Char

type Token = String
--note that a token will never be empty!

--clex will take the source code (as a long string) and produce a set of tokens
clex :: String -> [Token]
clex (c:cs)
    | isSpace c = clex cs
    | isDigit c = numToken : rest
        where
            numToken = takeWhile isDigit cs
            rest     = dropWhile isDigit cs

{-
--once we have the tokens, we can then perform syntactical analysis
syntax :: [Token] -> CoreProgram

parse :: String -> CoreProgram
parse = syntax . clex
-}
