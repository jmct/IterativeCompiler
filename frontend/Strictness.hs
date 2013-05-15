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


abs :: Expr Name -> Expr Name
abs (EVar n) = EVar n
abs (ENum v) = eTop
abs (EConstrAp t a args) = eTop
abs (EVar f `EAp` a1 `EAp` a2)
    | f `elem` (aDomain builtInDyadic) = conj (abs a1) (abs a2)
abs (ECase sub args) = conj sub (
