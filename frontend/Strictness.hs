module Strictness where

import Data.List 
import Language
import Parser
import Heap

fst3  (a, b, c) = a
snd3  (a, b, c) = b
thrd3 (a, b, c) = c


--Basic Values for abstract interpretation

--Top and Bottom
eBottom = ENum 0

eTop = ENum 1

--Conjunction and Disjunction
conj :: Expr Name -> Expr Name -> Expr Name
conj arg1 arg2 = EVar "&" `EAp` arg1 `EAp` arg2

disj :: Expr Name -> Expr Name -> Expr Name
disj arg1 arg2 = EVar "|" `EAp` arg1 `EAp` arg2

{- Currently our disjAppChain does NOT make the chain like in the diagram below
 -  In order for it to do so the disj function would have to be defined as:
 -
 -           disj :: Expr Name -> Expr Name -> Expr Name
 -           disj arg1 arg2 = EAp (EVar "|") (EAp arg1  arg2)
 -
 -  But this would be different to how App chains are currently...
 -}

--disjAppChain creates an expression of the form:
--
--        App (Var "|") (App alt1 (App (Var "|") (App alt2 (App ...
--
-- This creates an AST of the form:
--            
--                     @
--                    / \
--                  "|"  @
--                      / \
--                    alt  @
--                        / \
--                      "|"  @
--                          / \
--                        alt eBottom
--
disjAppChain :: [Expr Name] -> Expr Name
disjAppChain = foldl disj eBottom

--Replace all instances of the name provided as 'old' with the expression provided 
--as 'sub'. This will not lift the expression into the abstract domain. This only
--swaps out names.
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


--This is the function that lifts an expression into the abstract domain. 
--Right now our domain is a two point domain {Bottom, Top}
abstract :: Expr Name -> Expr Name
abstract (EVar n) = EVar n
abstract (ENum v) = eTop
abstract (EConstrAp t a args) = eTop
abstract (EVar f `EAp` a1 `EAp` a2)
    | f `elem` builtInDyadicNames = conj (abstract a1) (abstract a2)
abstract (ECase sub args) = conj sub (abstractAlts args)
abstract (ELet r bndgs expr) =
    ELet r (zip (map fst bndgs) (map (abstract . snd) bndgs)) $ abstract expr
abstract (EAp expr1 expr2) = EAp (abstract expr1) (abstract expr2)



--abstractAlts converts a list of case alternatives into an App Chain 
--using disjAppChain. It must first replace all instances of variables
--introduced as Constructor fields with 'eTop'
abstractAlts :: [Alter Name] -> Expr Name
abstractAlts alts = disjAppChain $ map abstract alts'
    where 
      alts'        = map subInAlt alts
      subInAlt alt = foldr (subInExpr eTop) (thrd3 alt) (snd3 alt)
