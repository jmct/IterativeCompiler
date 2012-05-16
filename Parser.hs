module Parser where

import Language
import Data.List
import Data.Char

type Token = (Int, String)
--note that a token will never be empty!

first :: a -> b -> a
first a b = a

second :: a -> b -> b
second a b = b

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

keywords :: [String]
keywords = ["let","letrec","case","in","of","Pack"]

--Two character operators must be a member of the following string.
--Currently the Not-Equal-To operator is ~= This may be changed to /=
twoCharOps :: [String]
twoCharOps = ["==","~=",">=","<=","->"]

relOps :: [String]
relOps = "<" : ">" : (delete "->" twoCharOps)

isTwoCharOp :: String -> Bool
isTwoCharOp str = (take 2 str) `elem` twoCharOps

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\r"

isComment :: String -> Bool
isComment ('-':'-':rest) = True
isComment _ = False

--We define a Parser as a function that takes a list of tokens and returns
--a tuple of the parsed value and the remaining list of tokens.
type Parser a = [Token] -> [(a, [Token])]

--For parsing infix operators we need a type for partial expressions
--NoOp is for when the parsing of an infix fails
data PartialExpr = NoOp | FoundOp Name CoreExpr

--For Literal values we will take a string and a Parser for type String and 
--compare the provided string with the value of the fist token's String
--if this fails we return an empty list, signalling that the parsing has failed. 
pLiteral :: String -> Parser String
pLiteral _ []       = []
pLiteral s (tok:toks)
    | s == snd tok  = [(s, toks)]
    | otherwise     = []

--For variables we have to check if the token is a vairable or not.
--Therefore we don't need to provide any other information other than the token
pVar :: Parser String
pVar []     = []
pVar (tok:toks) 
    | isVarToken $ snd tok = [(snd tok, toks)]
    | otherwise              = []
        where
            isVarToken s = (isAlpha.head $ s) && (s `notElem` keywords)

--pAlt is used to provide the result of two parsers for when there can be two
--valid parsings of a token. 
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

--We can generalize pLiteral and pVar with pSat that checks is the token
--satifies some property given as a parameter.
pSat :: (String -> Bool) -> Parser String
pSat _ [] = []
pSat p (tok:toks)
    | (p.snd) tok = [(snd tok, toks)]
    | otherwise   = []


--pNum to recognize Ints
pNum :: Parser Int
pNum = pApply (pSat (isDigit.head)) (read)
--pThen takes two parsers, p1 and p2, performs the parsing with p1 then parses
--the remaining list of tokens (from p1's result) 
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combiner p1 p2 toks = 
        [(combiner rslt1 rslt2, toks2) | (rslt1, toks1) <- p1 toks,
                                         (rslt2, toks2) <- p2 toks1]

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combiner p1 p2 p3 toks = 
        [(combiner rslt1 rslt2 rslt3, toks3) | (rslt1, toks1) <- p1 toks,
                                               (rslt2, toks2) <- p2 toks1,
                                               (rslt3, toks3) <- p3 toks2]

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c 
                                              -> Parser d -> Parser e
pThen4 combiner p1 p2 p3 p4 toks = 
        [(combiner rslt1 rslt2 rslt3 rslt4, toks4) | (rslt1, toks1) <- p1 toks,
                                                     (rslt2, toks2) <- p2 toks1,
                                                     (rslt3, toks3) <- p3 toks2,
                                                     (rslt4, toks4) <- p4 toks3]
--pZeroOrMore will take a parser and return a parser that recognizes zero or
--more of whatever the given parser recoginizes (unlike the usual zero or one).
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p1 = (pOneOrMore p1) `pAlt` (pEmpty [])

pEmpty :: a -> Parser a
pEmpty a toks = [(a, toks)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p1  = pThen (:) (p1)
                           (pZeroOrMore p1) 

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 pS = pThen (:) p1 (pZeroOrMore $ pThen second pS p1)

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f = \toks -> [((f a), toks1) | (a, toks1) <- p toks]


pEnd :: Parser ()
pEnd = \toks -> if toks == [] then  [((), [])]
                              else []
pSep :: String -> Parser String
pSep sep = pLiteral sep

--Silly test parsers
--
--
--Silly test parsers
pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLiteral "Hello") `pAlt` (pLiteral "Goodbye")

pGreeting :: Parser (String, String)
pGreeting = pThen (,) pHelloOrGoodbye pVar

pGreeting' :: Parser (String, String)
pGreeting' = pThen keepFrst 
                    (pThen (,) pHelloOrGoodbye pVar)
                    (pLiteral "!")
                where
                    keepFrst a b = a

pGreeting'' :: Parser (String, String)
pGreeting'' = pThen3 mkGreeting pHelloOrGoodbye pVar (pLiteral "!")
    where
        mkGreeting a b c = (a,b)

pGreetings :: Parser [(String, String)]
pGreetings = pThen (\x y -> x) (pZeroOrMore pGreeting) (pEnd)

pGreetingsN :: Parser Int
pGreetingsN = (pGreetings) `pApply` length
--once we have the tokens, we can then perform syntactical analysis
syntax :: [Token] -> CoreProgram
syntax = takeFirstParse . pProgram
    where
        takeFirstParse ((prog, []):otherParses) = prog
        takeFirstParse (parse:otherParses) = takeFirstParse otherParses
        takeFirstParse _ = error "Syntax error!"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLiteral ";")

pSc :: Parser (ScDef Name)
pSc = pThen4 makeSc pVar (pZeroOrMore pVar) (pLiteral "=") pExpr
    where
        makeSc name args eq expr = (name, args, expr)        

pFull f = pThen (\x y -> x) f (pEnd)

--------------------------------------------------------------------------------
--The parser functions below are for the grammar outlined in Figure 1.1 of IFL

{-For parsing the infix operators we break up the parsing of expressions into
 - levels of precedence. The grammar below makes the precedence explicit:
 - 
 - expr -> let defs in expr
 -       | letrec defs in expr
 -       | case expr of alts
 -       | \var1 ... varn . expr
 -       | expr1
 -
 - expr1  -> expr2 expr1c
 - expr1c -> | expr1        --This means that expr1c is either an OR followed
 -         | []             --by an expr1 or an empty list. This, and all
 -                          --similar cases below require partial expressions
 -}
pExpr1 :: Parser CoreExpr
pExpr1 = pThen assembleOp pExpr2 pExpr1c

pExpr1c :: Parser PartialExpr
pExpr1c = (pThen FoundOp (pLiteral "|") pExpr1) `pAlt` (pEmpty NoOp)

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2
{-
 - expr2  -> expr3 expr2c
 - expr2c -> & expr2
 -         | []
 -}
pExpr2 :: Parser CoreExpr
pExpr2 = pThen assembleOp pExpr3 pExpr2c

pExpr2c :: Parser PartialExpr
pExpr2c = (pThen FoundOp (pLiteral "&") pExpr2) `pAlt` (pEmpty NoOp)

{- expr3  -> expr4 expr3c
 - expr3c -> relop expr3
 -         | []
 -}
pExpr3 :: Parser CoreExpr
pExpr3 = pThen assembleOp pExpr4 pExpr3c

pExpr3c :: Parser PartialExpr
pExpr3c = (pThen FoundOp pRelOp pExpr3) `pAlt` (pEmpty NoOp)

pRelOp :: Parser String
pRelOp = pSat (`elem` relOps)

{- expr4  -> expr5 expr4c
 - expr4c -> + expr4
 -         | - expr5
 -         | []
 -}
pExpr4 :: Parser CoreExpr
pExpr4 = pThen assembleOp pExpr5 pExpr4c

pExpr4c :: Parser PartialExpr
pExpr4c = (pThen FoundOp (pLiteral "+") pExpr4) `pAlt` 
          (pThen FoundOp (pLiteral "-") pExpr5) `pAlt`
          (pEmpty NoOp)

{- expr5  -> expr6 expr5c
 - expr5c -> * expr5
 -         | / expr6
 -         | []
 -}
pExpr5 :: Parser CoreExpr
pExpr5 = pThen assembleOp pExpr6 pExpr5c

pExpr5c :: Parser PartialExpr
pExpr5c = (pThen FoundOp (pLiteral "*") pExpr5) `pAlt` 
          (pThen FoundOp (pLiteral "/") pExpr6) `pAlt`
          (pEmpty NoOp)

{- expr6  -> aexpr1 ... aexprN   where N >= 1
 - -}
pExpr6 :: Parser CoreExpr
pExpr6 = pConstr `pAlt` pApplication

{-The parsing for Let expressions and Recursive-Let expressions are defined
--independently, this is because the core Language uses different keywords for
--the two. -}
{-PExpr will string together all of parsers for the valid expressions as defined in Language.hs
 -There will be one parser for each expression type and a few helper functions/parsers -}
pExpr :: Parser CoreExpr
pExpr = pLet `pAlt` pLetRec `pAlt` pCase `pAlt` pLambda 
        `pAlt` pExpr1

pAexpr :: Parser CoreExpr
pAexpr = pVarExpr `pAlt` pNumExpr `pAlt` pParen

pParen :: Parser CoreExpr
pParen = pThen3 retEx (pLiteral "(") pExpr (pLiteral ")")
        where
            retEx paren1 expr paren2 = expr

pLet :: Parser CoreExpr
pLet = pThen4 makeLet (pLiteral "let") pDefsWithSep (pLiteral "in") pExpr
        where
            makeLet leht defs inn expr = (ELet False defs expr)

pLetRec :: Parser CoreExpr
pLetRec = pThen4 makeLet (pLiteral "letrec") pDefsWithSep (pLiteral "in") pExpr
        where
            makeLet leht defs inn expr = (ELet True defs expr)

pDef :: Parser (Name, Expr Name)
pDef = pThen3 makeDef pVar (pLiteral "=") pExpr
    where
        makeDef var eqs expr = (var, expr)

pDefsWithSep :: Parser [(Name, Expr Name)]
pDefsWithSep = pOneOrMoreWithSep pDef (pLiteral ";")

{-The parsing of the case expression relies on the processing of the necessary 
 - keywords (case, of) and parsing of the alternatives, which take the form:
 -
 - alt1;alt2;alt3...altn        n >= 1
 -
 - where each alternative takes the form of:
 -
 - <num> var1 var2 var3 ... varN -> Expr    N >=0
 -}
pCase :: Parser CoreExpr
pCase = pThen4 makeCase (pLiteral "case") pExpr (pLiteral "of") pCaseAlters
        where
            makeCase cse expr f alters = (ECase expr alters) 

pCaseAlters :: Parser [(Int, [Name], Expr Name)]
pCaseAlters = pOneOrMoreWithSep pAlter  (pLiteral ";")

--The combiner function just needs to take the parsed values, ignore the keywords
--and place them in a tuple. 
pAlter :: Parser (Int, [Name], Expr Name)
pAlter = pThen4 retCase pCaseNum pCaseVars (pLiteral "->") pExpr
        where
            retCase num vars arrow expr = (num, vars, expr)

pCaseNum :: Parser Int
pCaseNum = pThen3 retNum (pLiteral "<") pNum (pLiteral ">")
        where
            retNum brack num brack2 = num

pCaseVars :: Parser [String]
pCaseVars = pZeroOrMore pVar

pVarExpr :: Parser CoreExpr
pVarExpr = pApply pVar EVar

pNumExpr :: Parser CoreExpr
pNumExpr = pApply pNum ENum
{-Lambda expressions take a fairly simple form:
 -
 -\ var1 var2 var3 ... varN . expr     N >= 1
 -
 -So all that needs to be done is to parse the '\' out, 
 -then parse the variables in a manner identical to pCaseVars
 -then parse the literal '.' and then the body of the expression
 -(which is an expression
 -}
pLambda :: Parser CoreExpr
pLambda = pThen4 retLambda (pLiteral "\\") pLambVars (pLiteral ".") pExpr
        where
            retLambda lamb vars dot expr = (ELam vars expr)

pLambVars :: Parser [Name]
pLambVars = pOneOrMore pVar

pApplication :: Parser CoreExpr
pApplication = ((pOneOrMore pAexpr) `pApply` mkApChain) 

mkApChain :: [CoreExpr] -> CoreExpr
mkApChain (x:[]) = x
mkApChain (x1:x2:xs) = mkApChain (EAp x1 x2 : xs)

{-To parse constructors we must strip out the intergers from the Pack{num, num}
 - form. We do this by breaking the task into three parsers (note that with a
 - monadic parser we would only have needed one as pThen4 wouldn't be our
 - maximum parser-combinator). The first parser simple takes out the keyword
 - "Pack", the second parser parses the form of the curly brackets around the
 - two ints (which represent the constructor's tag and arity respectively), and
 - the last parser puts the ints into a tuple (ignoring the comma in the
 - Pack{int1,int2} form)-}
pConstr :: Parser CoreExpr
pConstr = pThen3 makeConstr (pLiteral "Pack") pBrackets pConstrArgs
        where
            makeConstr _ (num1,num2) args = EConstrAp num1 num2 args
            pConstrArgs                   = pZeroOrMore pAexpr

pBrackets :: Parser (Int, Int)
pBrackets = pThen3 onlyTuple (pLiteral "{") pTagArity (pLiteral "}")
        where
            onlyTuple _ (x,y) _ = (x,y)

pTagArity :: Parser (Int, Int)
pTagArity = pThen3 makeTuple pNum (pLiteral ",") pNum
        where
            makeTuple num1 _ num2 = (num1, num2)

parse :: String -> CoreProgram
parse = syntax . clex 1

