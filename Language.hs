module Language where


type Name = String
type IsRec = Bool

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

