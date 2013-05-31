module Interp where

import Language

data Val = Val :@: Val
         | Constr Int Int [Val]
         | Var String
         | Num Int
         | Case Val [VAlts]
         | Lambda (Val -> Val)
         | TempLambda String Val
         | Let Bool Bindings Val

type VAlts = (Int, Name, Val)

type Bindings = (Name, Val)

instance Show Val where
    show (a :@: b) = "Ap: (" ++ show a ++ ") (" ++ show b ++ ")"
    show (Constr tag arity fields) 
        = "Ctr " ++ ' ':(show tag) ++ ' ':(show arity) ++ ' ':(concatMap show fields)
    show (Var s) = s
    show (Num n) = show n
    show (Case v alts) = "Case: "
    show (Lambda f) = "Lambda Function"
    show _ = "Default"

letToLambda :: Expr a -> Expr a
letToLambda (ELet False defs e) = foldr bindingToLambda e defs

bindingToLambda :: (a, Expr a) -> Expr a -> Expr a
bindingToLambda (n, expr) e = (ELam n e) `EAp` expr
