module Parser where

import Language
import Char

type Token = (Int, String)
--note that a token will never be empty!

{-clex will take the source code (as a long string) and produce a set of tokens 
 - the whitespace is 'eaten up' first with a check to isSpace, then numbers are
 - are taken in their entirety and lastly we have variables. Variables must
 - start with a character but can then contain anything.-}
clex :: Int -> String -> [Token]
clex _ [] = []
clex line (c:cs)
    | c == '\n' = clex (line + 1) cs
    | isWhiteSpace c = clex line cs
    | isDigit c = (line, numToken) : clex line rest_digit
    | isAlpha c = (line, varToken) : clex line rest_alpha 
    | isComment (c:cs) = clex (line + 1) rest_ignrCmnt
    | isTwoCharOp (c:cs) = (line, twoChrOpTkn) : clex line restTwoChar 
    | otherwise  = (line, [c]) : clex line cs
        where
            numToken       = c : takeWhile isDigit cs
            rest_digit     = dropWhile isDigit cs
            varToken       = c : takeWhile isIdChar cs
            rest_alpha     = dropWhile isIdChar cs
            rest_ignrCmnt  = drop 1 $ dropWhile (/= '\n') cs --need to drop the \n too
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
isWhiteSpace c = c `elem` " \t\r"

isComment :: String -> Bool
isComment ('-':'-':rest) = True
isComment _ = False




{-
--once we have the tokens, we can then perform syntactical analysis
syntax :: [Token] -> CoreProgram

parse :: String -> CoreProgram
parse = syntax . clex
-}
