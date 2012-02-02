module Language where


type Name = String
type IsRec = Bool

type Alter a = (Int, [a], Expr a)

data Expr a
    = EVar Name
    | ENum Int
    | EConstr Int Int
    | EAp (Expr a) (Expr a)
    | ELet
        IsRec
        [(a, Expr a)]
        (Expr a)
    | ECase
        (Expr a)
        [Alter a]
    | ELam [a] (Expr a)
    deriving Show
