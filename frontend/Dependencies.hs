module Dependencies where

import Language
import Val
import Data.List

type Id = String

type Decl = (Name, [Name], Val)

type Binding = (Name, Val)

-- Depdendency graphs
type DepGraph = [(Id, [Id])]

--Get the list of function names that the passed function depends on
depends :: DepGraph -> Id -> [Id]
depends g f = case lookup f g of { Nothing -> [] ; Just gs -> gs }

--Finding the mutually dependent groups of functions
----------------------------------------------------
closure :: DepGraph -> DepGraph
closure g = fixPoint step g

step :: DepGraph -> Maybe DepGraph
step g
  | any snd joined = Just (map fst joined)
  | otherwise = Nothing
  where joined = map (join g) g

join :: DepGraph -> (Id, [Id]) -> ((Id, [Id]), Bool)
join g (f, fs) = ((f, reached), length fs < length reached)
  where reached = nub (fs ++ concatMap (depends g) fs)

fixPoint :: (a -> Maybe a) -> a -> a
fixPoint f a = case f a of { Nothing -> a ; Just b -> fixPoint f b }

-- For each function, determine all functions it calls
callGraph :: [Decl] -> DepGraph
callGraph p = (zip fs cs)
  where
    fs = map fst3 p
    cs = map (nub . calls . thrd3) p

calls :: Val -> [String]
calls val = nub $ go [] val
    where 
        go vars (Fun f) = [f]
        go vars (Case e alts) = go vars e ++ concat [go vars (thrd3 alt) | alt <- alts]
        go vars (Let ir bnds expr) = go vars expr ++ concatMap (go vars . snd) bnds
        go vars (Var name) = []
        go vars (Ap a bs)  = go vars a ++ concatMap (go vars) bs
        go vars e = vars

-- Strongly connected components, in dependency order
components :: DepGraph -> [[Id]]
components g = order g (comps (closure g) (map fst g))

comps :: DepGraph -> [Id] -> [[Id]]
comps g [] = []
comps g (f:fs)
  | null hs = [f] : comps g fs
  | otherwise = hs : comps g (filter (`notElem` hs) fs)
  where hs = [h | h <- depends g f, f `elem` depends g h]

order :: DepGraph -> [[Id]] -> [[Id]]
order g xs = ord [] xs
  where
    allIds = map fst g

    ord seen [] = []
    ord seen xs = free ++ ord (concat free ++ seen) rest
      where
        (free, rest) = partition p xs
        p ys = all (independent ys) (concatMap (depends g) ys)
        independent ys z = z `elem` ys || z `notElem` allIds || z `elem` seen

--Given a function name, return the declaration
lookupFunc :: [Decl] -> Name -> Decl
lookupFunc prog id
    | null passed = error $ "Tried looking up a function that does not exist: " ++ id
    | otherwise   = head passed
        where passed = [(name, args, expr) | (name, args, expr) <- prog, name == id]


-- Group function definitions into recursive call groups
callGroups :: [Decl] -> [[Decl]]
callGroups p = map (map (lookupFunc p)) (components (callGraph p))

-- For each let binding, determine all other let bindings it depends on
letGraph :: [Binding] -> DepGraph
letGraph bs = zip vs (map (filter (`elem` vs) . freeVariables) es)
  where (vs, es) = unzip bs

-- Group lets into recursive binding groups
letGroups :: [Binding] -> [[Binding]]
letGroups bs = map (map (lookupBinding bs)) (components (letGraph bs))

lookupBinding :: [(Id, Val)] -> Id -> (Id, Val)
lookupBinding bs w = 
  case lookup w bs of
    Nothing -> error $ "Dependency: lookupBinding: " ++ w
    Just e -> (w, e)
