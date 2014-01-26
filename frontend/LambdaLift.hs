module LambdaLift (lambdaLift, freeVars) where

import Language
import Data.Set

type AnProgram = AnProg Name (Set Name)

lambdaLift :: CoreProgram -> CoreProgram
lambdaLift = collectSCs . rename . abstract . freeVars

--Find the free variables in each expression and annotate the 
--AST
freeVars :: CoreProgram -> AnProgram


freeVarsOf :: AnExpr Name (Set Name)
freeVarsOf = fst

--Get the body expression from the list of bindings for a Let
letBodies = snd

--Determine the free variables of an expression
freeVarsExpr :: Set Name -> CoreExpr -> AnExpr Name (Set Name)
freeVarsExpr s (ENum n) = (empty, ANum n)
freeVarsExpr s (EVar n)
    | n `member` s = (singleton n, AVar n)
    | otherwise    = (empty, AVar n)
freeVarsExpr s (EAp e1 e2) = (s', AAp e1' e2')
  where
    e1' = freeVarsExpr s e1
    e2' = freeVarsExpr s e2
    s'  = (freeVarsOf e1') `union` (freeVarsOf e2')
freeVarsExpr s (ELam args expr) = (s', ALam args expr')
  where
    argsSet = fromList args
    expr'   = freeVarsExpr (s `union` argsSet) expr
    s'      = difference (freeVarsOf expr') argsSet
freeVarsExpr s (ELet r bins body) = (defsFree `union` bodyFree, ALet r defs' body')
  where
    binders   = bindersOf bins
    binderSet = fromList binders
    bodyVars  = s `union` binderSet
    rhsVars
        | r         = bodyVars
        | otherwise = s
    rhss'     = map (freeVarsExpr rhsVars) (letBodies bins)
    defs'     = zip binders rhss'
--TODO


--Take all lambda abstractions and make them into let-bindings
abstract :: AnProgram -> CoreProgram 

--Give each variable a unique name
rename :: CoreProgram -> CoreProgram


collectSCs :: CoreProgram -> CoreProgram


