module Val where

import Data.List 
import Language
import Parser
import Heap

fst3  (a, b, c) = a
snd3  (a, b, c) = b
thrd3 (a, b, c) = c

data Point4 = Top4        --All elements and entire spine defined
            | SpineStrict --Entire Spine defined, but some elements may be _|_
            | Inf         --Spine either has a bottom or is infinite
            | Bottom4     --No elements and no spine
    deriving Show

data Val = Top | Bottom            --For Flat Domains
         | Val4 Point4             --For Lists
         | Var String              --Variables in expressions
         | Fun String              --Function names
         | Constr Int Int [Val]    --Constructors
         | Ap Val [Val]            --Vectorised Applications
         | Let Bool [(String, Val)] Val
         | Case Val [VAlt]
    deriving Show

type VAlt = (Int, [String], Val)

--Apply a function to the next level 'down' of an expression
descendVal :: (Val -> Val) -> Val -> Val
descendVal f (Constr t a flds) = Constr t a $ map f flds
descendVal f (Ap a bs) = Ap (f a) (map f bs)
descendVal f (Let is bndgs expr) = Let is bndgs' (f expr)
    where bndgs' = [(n, f rhs) | (n, rhs) <- bndgs]
descendVal f (Case sub alts) = Case (f sub) alts'
    where alts' = [(i, args, f expr) | (i, args, expr) <- alts]
descendVal f e = e

--All the free variables of an Expression
freeVariables :: Val -> [String]
freeVariables val = nub $ go [] val
    where 
        go vars (Case e alts) = go vars e ++ concat [go (snd3 alt ++ vars) (thrd3 alt) | alt <- alts]
        go vars (Let ir bnds expr) = let newVars = map fst bnds ++ vars
                                  in go newVars expr ++ concatMap (go newVars . snd) bnds
        go vars (Var name) = [name | name `notElem` vars]
        go vars (Ap a bs)  = go vars a ++ concatMap (go vars) bs
        go vars e = vars

