module Language where
--import Debug.Trace
import List

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
isAtomicExpr e = False     -- If the pattern matching gets here, it's false

--Supercombinators definitions contain the name of the supercombinator,
--the list of arguments and the expression (body) to compute.
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
                ("S", ["f", "g", "x"], EAp (EAp (EVar "f") 
                                                (EVar "x")) 
                                           (EAp (EVar "g") 
                                                (EVar "x"))),
                ("compose", ["f", "g", "x"], EAp (EVar "f") 
                                                 (EAp (EVar "g") 
                                                      (EVar "x"))),
                ("twice", ["f"], EAp (EAp (EVar "compose") 
                                          (EVar "f")) 
                                     (EVar "f")) ]

{-**************************
 - Pretty Printing a program
 - *************************-}

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 $ take n e2s
    where e2s = e2 : e2s

--The data-type Iseq is what is going to be used to flatten
--the large printing tree when we want to pretty-print

data Iseq = INil 
          | IStr String 
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline
{-
IIndent seq = seq

INewline = IStr "\n"
-}
{-The flatten function takes a list of ISeqs and creates one long 
 - string that is the printed function. You may be wondering, 'But 
 - we don't have a list of ISeqs, we only have the resulting (large)
 - ISeq from pprProgram. What gives?'. Well, the flatten function
 - actually _builds_ builds up the list of ISeqs *as* it consumes it.
 - This is why we've put off the work of appending strings this entire time,
 - so we could do it all at once. 
 -
 - The Int that is passed to flatten is used to keep track of the current
 - column for indentation -}
flatten :: Int -> [(Iseq, Int)] -> String
flatten col [] = ""
flatten col ((INil, indent) : seqs) = flatten col seqs
flatten col ((IStr a, indent) : seqs) = a ++ flatten (col + length a) seqs
flatten col ((IAppend iseq1 iseq2, indent) : seqs) = 
    flatten col ((iseq1, indent) : (iseq2, indent) : seqs)
flatten col ((INewline, indent) : seqs) = 
    '\n' : (take indent $ repeat ' ') ++ (flatten indent seqs)
flatten col ((IIndent seq, indent) : seqs) = 
    flatten col ((seq, col) : seqs) 





--iDisplay takes the Iseq representation of the program and passes it to
--flatten. The zero is for the starting column (most programs start on the
--leftmost column).
iDisplay seq = flatten 0 [(seq, 0)]



--First we define how we would print our CoreExpressions
-------------------------------------------------------------------------------
pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n) = IStr $ show n
pprExpr (EVar v) = IStr v
pprExpr (ELet isrec defs expr) = 
    iConcat [ IStr keyword, INewline, IStr " ", 
              IIndent (pprLetDefs defs),
              INewline, IStr "in ", pprExpr expr ]
    where keyword = if isrec then "letrec" else "let"
pprExpr (EAp e1 e2) = 
    (pprExpr e1) `IAppend` (IStr " ") `IAppend` (pprExprParen e2)
    where pprExprParen e = 
            if isAtomicExpr e then pprExpr e
            else (IStr "(") `IAppend` (pprExpr e `IAppend` (IStr ")"))
pprExpr (ECase (e1) alts) = decla `IAppend` alters
    where 
        decla = iConcat [ IStr "Case ", pprExpr e1, IStr " of", INewline ]
        alters = iConcat [ IStr " ", IIndent (pprAlters alts) ]
pprExpr (ELam vars e1) = iConcat [IStr "\\", 
                                  IStr (concat $ intersperse " " vars), 
                                  IStr " . ", (pprExpr e1)]

pprProgram :: CoreProgram -> Iseq
pprProgram [] = INil
pprProgram (supComb:rest) = (pprDef supComb) `IAppend` (pprProgram rest)

pprint prog = iDisplay (pprProgram prog)

--The following are for supercombinator Definitions
pprDef :: ScDef Name -> Iseq
pprDef (name, args, expr) = iConcat [IStr name, IStr " ",
                                     iInterleave (IStr " ") (map IStr args),
                                     IStr "= ", IIndent (pprExpr expr),
                                     INewline, INewline]

--The following functions are for pretty-printing the Let definitions
pprLetDefs :: [(Name, CoreExpr)] -> Iseq
pprLetDefs defs = iInterleave sep (map pprLetDef defs)
    where sep = iConcat [IStr ";", INewline]

pprLetDef :: (Name, CoreExpr) -> Iseq
pprLetDef (name, expr) = iConcat [IStr name, IStr " = ", 
                                  IIndent (pprExpr expr)]

pprAlters :: [CoreAlt] -> Iseq
pprAlters [] = INil
pprAlters (x:xs) = (pprAlter x) `IAppend` 
                            INewline `IAppend` (pprAlters xs)

pprAlter :: CoreAlt -> Iseq
pprAlter ( _, vars, e1) = 
    iConcat [IStr (concat.intersperse " " $ map show vars), 
             IStr " -> ", (pprExpr e1)]

iConcat :: [Iseq] -> Iseq
iConcat []     = INil
iConcat (x:xs) = x `IAppend` iConcat xs

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ []       = INil
iInterleave sep (x:xs) = (x `IAppend` sep) `IAppend`  (iInterleave sep xs)

--pprint :: CoreProgram -> String
