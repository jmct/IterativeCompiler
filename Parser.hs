module Parser where

import Language
import Char

type Token = String
--note that a token will never be empty!

{-clex will take the source code (as a long string) and produce a set of tokens 
 - the whitespace is 'eaten up' first with a check to isSpace, then numbers are
 - are taken in their entirety and lastly we have variables. Variables must
 - start with a character but can then contain anything.-}
clex :: String -> [Token]
clex [] = []
clex (c:cs)
    | isWhiteSpace c = clex cs
    | isDigit c = numToken : clex rest_digit
    | isAlpha c = varToken : clex rest_alpha 
    | isComment (c:cs) = clex rest_ignrCmnt
    | isTwoCharOp (c:cs) = twoChrOpTkn : clex restTwoChar 
    | otherwise  = [c] : clex cs
        where
            numToken       = c : takeWhile isDigit cs
            rest_digit     = dropWhile isDigit cs
            varToken       = c : takeWhile isIdChar cs
            rest_alpha     = dropWhile isIdChar cs
            rest_ignrCmnt  = dropWhile (/= '\n') cs
            twoChrOpTkn    = take 2 (c:cs)
            restTwoChar    = drop 2 (c:cs)

--Two character operators must be a member of the following string.
--Currently the Not-Equal-To operator is ~= This may be changed to /=
twoCharOps :: [String]
twoCharOps = ["==","~=",">=","<=","->"]

isTwoCharOp :: String -> Bool
isTwoCharOp str = (take 2 str) `elem` twoCharOps

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

isComment :: String -> Bool
isComment ('-':'-':rest) = True
isComment _ = False




{-
--once we have the tokens, we can then perform syntactical analysis
syntax :: [Token] -> CoreProgram

parse :: String -> CoreProgram
parse = syntax . clex
-}
