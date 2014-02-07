module CaseLift (caseLift) where

import Language
import Parser
import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Writer.Strict
import Data.Set as S hiding (map, foldl', partition)
import Data.List (foldl', partition)
import qualified Data.Map as M
import Fresh
import LambdaLift (freeVars, rename, collectSCs)

builtInDyadic :: [Name]
builtInDyadic
    = ["+", "-", "*", "/", 
       "==", "~=", ">=", "div",
       ">", "<=", "<"] 

type AnProgramEager = AnProg Name (Set Name)

caseLift :: CoreProgram -> CoreProgram
caseLift = collectSCs . rename . abstractCase . freeVars

freeVarsOf :: AnExpr Name (Set Name) -> Set Name
freeVarsOf = fst

--Get the body expression from the list of bindings for a Let
letBodies :: [(Name, Expr Name)] -> [Expr Name]
letBodies = map snd



--Take all case expressions in an non-eager context and convert them into let-bindings
abstractCase :: AnProg Name (Set Name) -> CoreProgram 
abstractCase prog = [ (name, args, abstractExprE rhs) | (name, args, rhs) <- prog ]

abstractExprE :: AnExpr Name (Set Name) -> CoreExpr
abstractExprE (_, AVar v)             = EVar v
abstractExprE (_, ANum n)             = ENum n
abstractExprE (_, ALet ir defs body)  = ELet ir (map abstractDef defs) $ abstractExprE body
abstractExprE (_, AConstrAp t a flds) = EConstrAp t a $ map abstractExprC flds
abstractExprE (_, ACase s alts)       = ECase (abstractExprE s) $ map abstractExprAlt alts
abstractExprE (_, AAp (_, AAp (_, AVar arith) e1) e2)
    | elem arith builtInDyadic = 
                EVar arith `EAp` abstractExprE e1 `EAp` abstractExprE e2
abstractExprE expr = abstractExprC expr


abstractExprAlt :: AnAlt Name (Set Name) -> CoreAlt
abstractExprAlt (t, vars, rhs) = (t, vars, abstractExprE rhs)

abstractDef :: AnDefn Name (Set Name) -> (Name, CoreExpr)
abstractDef (n, body) = (n, abstractExprC body)

abstractExprC :: AnExpr Name (Set Name) -> CoreExpr
abstractExprC (_, AVar v)             = EVar v
abstractExprC (_, ANum n)             = ENum n
abstractExprC (_, ALet ir def body)   = ELet ir (map abstractDef def) $ abstractExprC body
abstractExprC (_, AAp e1 e2)          = EAp (abstractExprC e1) (abstractExprC e2)
abstractExprC (_, AConstrAp t a flds) = EConstrAp t a $ map abstractExprC flds
abstractExprC (fvs, ACase s alts)     = foldl' EAp sc $ map EVar fvList
  where
    fvList = S.toList fvs
    sc     = ELet False [("sc", rhs)] (EVar "sc") 
    rhs    = ELam fvList $ ECase (abstractExprE s) $ map abstractExprAlt alts
