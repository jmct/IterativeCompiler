module Interp where

import Language
import Parser

data Val = Val :@: Val
         | Constr Int Int [Val]
         | Var String
         | Num Int
         | Case Val [VAlts]
         | Lambda (Val -> Val)
         | TempLambda String Val
--         | Let Bool Bindings Val

type VAlts = (Int, Val)

type Bindings = (Name, Val)

exprToVal :: Expr Name -> Val
exprToVal (a `EAp` b) = exprToVal a :@: exprToVal b
exprToVal (EConstrAp t a fields) = Constr t a $ map exprToVal fields
exprToVal (EVar t) = Var t
exprToVal (ENum t) = Num t
exprToVal (ECase s alts) = Case (exprToVal s) $ map exprToValCAlt alts
exprToVal (ELam n expr) = TempLambda n $ exprToVal expr
exprToVal e@(ELet isRec bndgs expr) = exprToVal $ letToLambda e

exprToValCAlt :: Alter Name -> VAlts
exprToValCAlt alt = lambdafyTriple alt


--Simple Lets that are not Letrecs are easy enough to convert to Lambdas
--Each binding becomes a Lambda abstraction of one variable that is applied
--to the definition body of the binding
--For example:

--One binding:
--  let x = 3 in (x * x) ==> (\x -> x * x) 3
--Two bindings:
--  let x = 3 
--      y = 4
--  in (x * y) ==> (\x -> (\y -> x * y) 4) 3
letToLambda :: Expr a -> Expr a
letToLambda (ELet False defs e) = foldr bindingToLambda e defs
letToLambda (ELet True defs e) 
    = error "Interpretation of recursive lets not yet supported, but they should be!" 

bindingToLambda :: (a, Expr a) -> Expr a -> Expr a
bindingToLambda (n, expr) e = (ELam n e) `EAp` expr

lambdafyTriple :: (a, [Name], Expr Name) -> (a, Val)
lambdafyTriple (n, args, expr) = (n, foldr lambdafyArg expr' args)
    where lambdafyArg name e = TempLambda name e
          expr' = exprToVal expr

mapOverSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapOverSnd f lst = zip cs $ map f $ as
    where (cs, as) = unzip lst

(@@) :: Val -> Val -> Val
(Lambda f) @@ x = f x
_ @@ _ = error "Trying to apply something that is not a function"


prims = [("S", Lambda $ \f -> Lambda $ \g -> Lambda $ \x -> (f @@ x) @@ (g @@ x))
        ,("K", Lambda $ \x -> Lambda $ \y -> x)
        ,("I", Lambda $ \x -> x)]

--These are the standard rules for compiling to SKI Combinators
compileToSKI :: Val -> Val
compileToSKI (a :@: b)              = compileToSKI a :@: compileToSKI b
compileToSKI (TempLambda name expr) = abstractSKI name (compileToSKI expr)
compileToSKI expr                   = expr

abstractSKI :: String -> Val -> Val
abstractSKI name (a :@: b) = (Var "S" :@: abstractSKI name a :@: abstractSKI name b)
abstractSKI name (Var v)
    | name == v = Var "I"
    | otherwise = Var "K" :@: Var v

instance Show Val where
    show (a :@: b) = "(" ++ show a ++ ") (" ++ show b ++ ")"
    show (Constr tag arity fields) 
        = "Ctr " ++ ' ':(show tag) ++ ' ':(show arity) ++ ' ':(concatMap show fields)
    show (Var s) = s
    show (Num n) = show n
    show (Case v alts) = "Case: " ++ show v
    show (Lambda f) = "Lambda Function"
    show (TempLambda n expr) = "\\" ++ n ++ " -> " ++ show expr
    show _ = "Default"


