module Language where
--import Debug.Trace


type Name = String
type IsRec = Bool

--For each alternative in a case statement we need the tag, list of bound
--vairables and the expression.
type Alter a = (Int, [a], Expr a)

data Expr a
    = EVar Name             --Variables (their name)
    | ENum Int              --Numbers
    | EConstr Int Int       --Constructors (the reference tag and the arity)
    | EAp (Expr a) (Expr a) --Application of expression
    | ELet                  --Let declaration
        IsRec               --Boolean (True == Recursive Let)
        [(a, Expr a)]       --list of definitions
        (Expr a)            --body of let
    | ECase                 --Case declaration
        (Expr a)            --expression to compare
        [Alter a]           --list of alternatives to execute
    | ELam [a] (Expr a)     --Lambda expression
    deriving Show

type CoreExpr = Expr Name   --An expression with its name
type CoreAlt = Alter Name   --An aternative expression and its name for cases

--Following two functions are to retrieve either the bounded variable names
--or the expressions from a list of definitions
bindersOf :: [(a,b)] -> [a]
bindersOf defs = [names | (names, exprs) <- defs]

expressionsOf :: [(a,b)] -> [b]
expressionsOf defs = [exprs | (names, exprs) <- defs]

--Atomic expressions are functions that have no structure on the RHS
--Because of our syntax, that means it is either an EVar or an ENum
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar a) = True
isAtomicExpr (ENum a) = True
isAtomicExpr e = False          -- If the pattern matching gets here, it's false

--Supercombinators definitions contain the name of the supercombinator, the list
--of arguments and the expression (body) to compute.
type ScDef a = (Name, [a], Expr a)

--Following from this, a program is just a list of supercombinators
type Program a = [ScDef a]

--A Core Program is the list of supercombinators with an included name
type CoreProgram = Program Name

{-********************************************************************
 - The Prelude for the Core Lanuage
 - 
 - The prelude will define the usual SKI combinators along with combinators
 - for composition (compose) and for repeating function application (twice).
 - *******************************************************************-}

preludeDefs :: CoreProgram
preludeDefs = [ ("I", ["x"], EVar "x"),
                ("K", ["x", "y"], EVar "x"), 
                ("K1", ["x", "y"], EVar "y"),
                ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x")) 
                                           (EAp (EVar "g") (EVar "x"))),
                ("compose", ["f", "g", "x"], EAp (EVar "f") 
                                                 (EAp (EVar "g") (EVar "x"))),
                ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]

{-**************************
 - Pretty Printing a program
 - *************************-}

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 $ take n e2s
    where e2s = e2 : e2s

--First we define how we would print our CoreExpressions

{- 
pprExpr :: CoreExpr -> String
pprExpr (ENum n) = show n
pprExpr (EVar v) = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprExprParen e2
                        where pprExprParen e = if isAtomicExpr e then pprExpr e
                                               else "(" ++ pprExpr e ++ ")"
-}

--
pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n) = show n
pprExpr (EVar v) = iStr v
pprExpr (ELet isrec defs expr) 
    = iConcat [ iStr keyword, iNewline, iStr " ", iIndent (pprDefs defs),
                iNewline, iStr "in ", pprExpr expr ]
    where keyword 
        | not isrec = "let"
        | isrec =     "letrec"
pprExpr (EAp e1 e2) = (pprExpr e1) 'iAppend' (iStr " ") 'iAppend' (pprExprParen e2)
                 where pprExprParen e = if isAtomicExpr e then pprExpr e
                                        else (iStr "(") 'iAppend' (pprExpr e 'iAppend' (iStr ")"))

pprDefs :: [(Name,CoreExpr)] 
pprDefs defs = iInterleave sep (map pprDefn defs)
    where sep = iConcat [iStr ";", iNewine] 

pprDef :: (Name, CoreExpr)
pprDef (name, expr) = iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]

iConcat :: [Iseq] -> Iseq
iConcat []     = iNil
iConcat (x:xs) = x 'iAppend' iConcat xs

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ []       = iNil
iInterleave sep (x:xs) = (x 'iAppend' sep) 'iAppend'  (iConcat xs)


data Iseq where
    isNil       :: Iseq
    iStr        :: String -> Iseq
    iAppend     :: Iseq -> Iseq -> Iseq
    iNewline    :: Iseq
    iIndent     :: Iseq -> Iseq
    iDisplay    :: Iseq -> String


--pprint :: CoreProgram -> String
