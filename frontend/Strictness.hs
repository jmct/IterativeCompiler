module Strictness where

import Data.List 
import Language
import Parser
import Val
import Dependencies
import Heap

{- Reminder of the AST:
 -
 -        data Point4 = Top4        --All elements and entire spine defined
 -                    | SpineStrict --Entire Spine defined, but some elements may be _|_
 -                    | Inf         --Spine either has a bottom or is infinite
 -                    | Bottom4     --No elements and no spine
 -            deriving Show
 -
 -        data Val = Top | Bottom            --For Flat Domains
 -                 | Val4 Point4             --For Lists
 -                 | Var String              --Variables in expressions
 -                 | Fun String              --Function names
 -                 | Constr Int Int [Val]    --Constructors
 -                 | Ap Val [Val]            --Vectorised Applications
 -                 | Let Bool [(String, Val)] Val
 -                 | Case Val [VAlt]
 -            deriving Show
 -
 -        type VAlt = (Int, [String], Val)
 -}

{-I Don't think these are needed anymore
twoPointBottom :: Val
twoPointBottom = Bottom

twoPointTop :: Val
twoPointTop = Top

fourPointBottom :: Val
fourPointBottom = Bottom

fourPointTop :: Val
fourPointTop = Top
-}

--Conjunction and Disjunction
conj :: Val -> Val -> Val
conj arg1 arg2 = Ap (Fun "&") [arg1, arg2]

disj :: Val -> Val -> Val
disj arg1 arg2 = Ap (Fun "|") [arg1, arg2]

--disjAppChain/conjAppChain creates an expression of the form:
--
--        Ap (Fun "|") [alt1, App (Fun "|") [alt2, (App ...
--
-- This creates an AST of the form:
--            
--                     @
--                    / \
--                  "|"  :
--                      / \
--                    alt  :
--                        / \
--                       @  []
--                      / \
--                    "|" [alt2, eBottom]
--
disjAppChain :: [Val] -> Val
disjAppChain = foldr disj Bottom

conjAppChain :: [Val] -> Val
conjAppChain = foldr conj Top

--Replace all instances of the name provided as 'old' with the expression provided 
--as 'sub'. This will not lift the expression into the abstract domain. This only
--swaps out names.
subInVal :: Val -> Name -> Val -> Val
subInVal sub old = subInVal'
    where subInVal' (Var n) | n == old = sub
          subInVal' (Fun n) | n == old = sub
          subInVal' (Constr t a exprs) = Constr t a $ map subInVal' exprs
          subInVal' (Ap expr1 exprs) = Ap (subInVal' expr1) (map subInVal' exprs)
          subInVal' (Let b defs expr) = 
                        let newDefs = zip (map fst defs) (map (subInVal' . snd) defs)
                        in Let b newDefs (subInVal' expr)
          subInVal' (Case subj alts) =
                        Case (subInVal' subj) [(x, y, subInVal' alt) | (x, y, alt) <- alts]
          subInVal' expr = expr

--For substituting many names in a list of expressions
subInMany :: Val -> [(Val, Name)] -> Val
subInMany = foldr (uncurry subInVal)

absAppChain :: Expr Name -> [Val] -> Val
absAppChain (EVar id `EAp` b) xs
    | id `elem` builtInDyadicNames = conj (abstract b) $ head xs
    | otherwise                   = Ap (Fun id) (abstract b:xs)
absAppChain (a `EAp` b) args = absAppChain a (abstract b:args)


abstractSubject :: Expr Name -> Val
abstractSubject (EConstrAp t a args) = Constr t a $ map abstract args
abstractSubject e = abstract e

--This is the function that lifts an expression into the abstract domain. 
--Right now our domain is a two point domain {Bottom, Top}
abstract :: Expr Name -> Val
abstract c@(a `EAp` b) = absAppChain c []
abstract (EVar n) = Var n
abstract (ENum v) = Top
abstract (EConstrAp t a args) = Top
abstract (ECase sub alts) = conj (abstractSubject sub) (abstractAlts alts)
abstract (ELet r bndgs expr) =
    Let r [(id, abstract def) | (id, def) <- bndgs] $ abstract expr



--abstractAlts converts a list of case alternatives into an App Chain 
--using disjAppChain. It must first replace all instances of variables
--introduced as Constructor fields with 'eTop'
abstractAlts :: [Alter Name] -> Val
abstractAlts alts = disjAppChain $ map abstract alts'
    where 
      alts'        = map subInAlt alts
      subInAlt alt = foldr (subInExpr (ENum 1)) (thrd3 alt) (snd3 alt)


{- Now that everything is abstracted, we perform some transformations -}

--For each let gather a list of all the other let definitions it needs
letDepends :: [(String, Val)] -> [(String, [String])]
letDepends bndgs = zip names $ map bndgAndFree exprs
    where (names, exprs) = unzip bndgs
          bndgAndFree = filter (`elem` names) . freeVariables

-- Inline let expressions in an abstract expression.  Looses sharing,
-- and assumes that all cyclic let bindings terminate. 
--Matt's version assumed ALL let bindings terminated, I don't know why... 
--There's probably a good reason.
inlineLet :: Val -> Val
inlineLet (Let ir [] e) = inlineLet e
inlineLet (Let ir [(v, rhs)] e)
  | v `elem` freeVariables rhs = inlineLet (subInVal Top v e)
  | otherwise = inlineLet (subInVal rhs v e)
inlineLet (Let ir bs e) = inlineLet (subInMany e s)
  where s = [(e, v) | (v, e) <- bs]
inlineLet e = descendVal inlineLet e

splitLet :: Val -> Val
splitLet (Let ir bs e) = foldr (Let ir) (inlineLet e) (letGroups bs')
  where bs' = [(v, inlineLet e) | (v, e) <- bs]
splitLet e = descendVal splitLet e

