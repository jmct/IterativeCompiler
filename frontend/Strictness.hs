module Strictness where

import Data.List 
import Language
import Parser
import Heap

--Basic Values for abstract interpretation

--Top and Bottom
eBottom = ENum 0

eTop = ENum 1

--Conjunction and Disjunction
conj :: Expr Name -> Expr Name -> Expr Name
conj arg1 arg2 = EVar "&" `EAp` arg1 `EAp` arg2

disj :: Expr Name -> Expr Name -> Expr Name
disj arg1 arg2 = EVar "|" `EAp` arg1 `EAp` arg2

subInExpr :: Expr Name -> Name -> Expr Name -> Expr Name
subInExpr sub old = subInExpr'
    where subInExpr' (EVar n) | n == old = sub
          subInExpr' (EConstrAp t a exprs) = EConstrAp t a $ map subInExpr' exprs
          subInExpr' (EAp expr1 expr2) = EAp (subInExpr' expr1) (subInExpr' expr2)
          subInExpr' (ELet b defs expr) = 
                        let newDefs = zip (map fst defs) (map (subInExpr' . snd) defs)
                        in ELet b newDefs (subInExpr' expr)
          subInExpr' (ECase subj alts) =
                        ECase (subInExpr' subj) [(x, y, subInExpr' alt) | (x, y, alt) <- alts]
          subInExpr' expr = expr


abstract :: Expr Name -> Expr Name
abstract (EVar n) = EVar n
abstract (ENum v) = eTop
abstract (EConstrAp t a args) = eTop
abstract (EVar f `EAp` a1 `EAp` a2)
    | f `elem` builtInDyadicNames = conj (abstract a1) (abstract a2)
--abstract (ECase sub args) = conj sub (
