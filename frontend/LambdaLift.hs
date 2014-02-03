module LambdaLift (lambdaLift, freeVars) where

import Language
import Parser
import Data.Set as S hiding (map, foldl')
import Data.List (foldl')
import qualified Data.Map as M
import Fresh

type AnProgram = AnProg Name (Set Name)

lambdaLift :: CoreProgram -> CoreProgram
lambdaLift = collectSCs . rename . abstract . freeVars

--Find the free variables in each expression and annotate the 
--AST
freeVars :: CoreProgram -> AnProgram
freeVars prog = [ (n, args, freeVarsExpr (fromList args) rhs)
                | (n, args, rhs) <- prog
                ]


freeVarsOf :: AnExpr Name (Set Name) -> Set Name
freeVarsOf = fst

--Get the body expression from the list of bindings for a Let
letBodies :: [(Name, Expr Name)] -> [Expr Name]
letBodies = map snd

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
    binders         = bindersOf bins
    binderSet       = fromList binders
    bodyVars        = s `union` binderSet
    rhsVars
        | r         = bodyVars
        | otherwise = s
    rhss'           = map (freeVarsExpr rhsVars) (letBodies bins)
    defs'           = zip binders rhss'
    freeInVal       = unions $ map freeVarsOf rhss'
    defsFree 
        | r         = difference freeInVal binderSet
        | otherwise = freeInVal
    body'           = freeVarsExpr bodyVars body
    bodyFree        = difference (freeVarsOf body') binderSet
freeVarsExpr s (ECase e alts) = freeVarsCase s e alts
freeVarsExpr s (EConstrAp t a args)  = (s', AConstrAp t a args')
  where
    args' = map (freeVarsExpr s) args
    s'    = unions $ map freeVarsOf args'

freeVarsCase s e alts = (s', ACase e' alts')
  where
    s'       = fv_e `union` fv_alts
    fv_e     = freeVarsOf e'
    e'       = freeVarsExpr s e
    fv_alts  = unions $ map freeVarsAlt alts'
    alts'    = freeVarsAlts s alts

freeVarsAlts :: Set Name -> [Alter Name] -> [AnAlt Name (Set Name)]
freeVarsAlts s alts = [ (t, args, freeVarsExpr (s `union` (S.fromList args)) rhs) 
                      | (t, args, rhs) <- alts
                      ]

freeVarsAlt :: AnAlt Name (Set Name) -> Set Name
freeVarsAlt (tag, args, rhs) = S.difference (freeVarsOf rhs) (S.fromList args)



--Take all lambda abstractions and make them into let-bindings
abstract :: AnProgram -> CoreProgram 
abstract prog = [ (name, args, abstractExpr rhs) | (name, args, rhs) <- prog ]

abstractExpr :: AnExpr Name (Set Name) -> CoreExpr
abstractExpr (_, AVar v) = EVar v
abstractExpr (_, ANum n) = ENum n
abstractExpr (_, AAp e1 e2) = EAp (abstractExpr e1) (abstractExpr e2)
abstractExpr (_, ALet ir defs body) = 
    ELet ir [ (name, abstractExpr rhs) | (name, rhs) <- defs ] (abstractExpr body)
abstractExpr (fvs, ALam args body) = foldl' EAp sc $ map EVar fvList
  where
    fvList = S.toList fvs
    sc     = ELet False [("sc", rhs)] (EVar "sc")
    rhs    = ELam (fvList ++ args) $ abstractExpr body
abstractExpr (_, AConstrAp t a flds) = EConstrAp t a $ map abstractExpr flds
abstractExpr (_, ACase s alts) = ECase (abstractExpr s) $ map abstractExprAlt alts

abstractExprAlt :: AnAlt Name (Set Name) -> CoreAlt
abstractExprAlt (t, vars, rhs) = (t, vars, abstractExpr rhs)


--Give each variable a unique name
rename :: CoreProgram -> CoreProgram
rename prog = snd $ runFresh (renameDecls prog) "v_" 0

renameDecls :: CoreProgram -> Fresh CoreProgram
renameDecls prog = mapM renameRhs prog

makeAssoc :: String -> Fresh (String, String)
makeAssoc str = fresh >>= (\frsh -> return (str, frsh))

renameRhs :: ScDef Name -> Fresh (ScDef Name)
renameRhs (scName, args, rhs) = do
    asscs <- mapM makeAssoc args
    let env = M.fromList asscs
        args' = map snd asscs
    rhs' <- renameExpr env
    return (scName, args', rhs')

renameExpr :: M.Map String String -> Fresh CoreExpr
renameExpr env = undefined

collectSCs :: CoreProgram -> CoreProgram
collectSCs = undefined


