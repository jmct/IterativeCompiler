module Compiler where

import Language
import Parser
import Heap
import Data.List

compileToGCode = labelProgram . compile . parse

writeGCodeFile name = 
    writeFile name . iDisplay . printInstructions . compileToGCode

{- 
 - Code for testing the compilation and labelling of G-Code:
 - readFile "prelude.src" >>= putStr. iDisplay . showInstructions . compileToGCode
 -
 - Typical compilation will look something like:
 -
 - readFile name >>= writeGCodeFile name
 -}

maximum' [] = 0
maximum' xs  = maximum xs


--mapAccuml is a utility function that will be used frequently in the code to
--come.
mapAccuml :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccuml f acc []      = (acc, [])
mapAccuml f acc (x:xs)  = (acc2, x':xs')
                where
                    (acc1, x')  = f acc x
                    (acc2, xs') = mapAccuml f acc1 xs

--The code for the GMCode type and `setter and getter' functions are below.
type GMCode = [Instruction]

data Instruction = 
          Unwind 
        | PushGlobal Name
        | PushInt Int
        | Push Int
        | MkAp
        | Update Int
        | Pop Int
        | Slide Int
        | Alloc Int
        | Eval                          --Forcing the eval of top of stack
        | Add | Sub | Mul | Div | Neg   --Arithmetic instructions
        | Eq | Ne | Lt | Le | Gt | Ge        --comparison instructions
        | Cond GMCode GMCode            --conditional with alternatives
        | Pack Int Int
        | Casejump [(Int, GMCode)]
        | CasejumpInstr String
        | Split Int
        | Print
        | Par
        | Label String
        | FunDef String
        | CaseAlt String
        | CaseAltEnd String
        | Case
    deriving Eq


--The code for the state's heap:
type GMHeap = Heap Node

--Globals are just an association list from the name of the function to the
--address in the heap. 
type GMGlobals = Assoc Name Addr

--A Node is either a number (a result), an application of two other nodes, or
--the arity & gCode sequence for when the global can be executed
data Node = 
          NNum Int
        | NAp Addr Addr
        | NGlobal Int GMCode
        | NInd Addr                           --Indirection node
        | NConstr Int [Addr]
    deriving Eq

{---This is the Monad for getting fresh names based on an initial String
data Fresh s a = Fresh { runFresh :: s -> (a, s) }

instance Monad (Fresh s) where
  return x = Fresh  $ (\s -> (x, s))
  (Fresh x) >>= f  = Fresh  $ \s -> let (res1, newState) = x s
                                        (Fresh g) = f res1
                                    in g newState

fresh :: Fresh String Int
fresh = Fresh $ (\s -> (0, s))




newFresh :: Int -> Fresh String Int
newFresh = \x -> Fresh $ (\s -> (x+1, s ++ show x))

----------------------------------------------------------------
--}
data CodeTree = Append CodeTree CodeTree
              | Nil
              | Code Instruction
--              | Codes [Instruction]
              
codeConcat :: [CodeTree] -> CodeTree
codeConcat [] = Nil
codeConcat (x:xs) = x `Append` codeConcat xs

flattenCode' :: CodeTree -> GMCode ->GMCode
flattenCode' (Nil) acc = acc
flattenCode' (Code c) acc = c : acc
flattenCode' (Append codeL codeR) acc = 
    flattenCode'  codeL (flattenCode' codeR acc)
    
    
flattenCode t = flattenCode' t []

data Fresh a = Fresh { runFresh :: String -> Int -> (Int, a) }

instance Monad Fresh where
  return a = Fresh (\s i -> (i, a))
  (Fresh h) >>= f = Fresh $ \s i -> let (i1, s1) = h s i
                                        (Fresh g) = f s1
                                        in g s i1

{- Naylor's bind implementation
  m >>= f  = Fresh (\s i -> case runFresh m s i of
                              (j, a) -> runFresh (f a) s j)
 -}

fresh :: Fresh String
fresh = Fresh (\s i -> (i+1, s ++ ": " ++ show i))

scfresh :: Fresh String
scfresh = Fresh (\s i -> (i+1, s ++ " " ++ show i))

ignore :: String -> Fresh a -> Fresh a
ignore str (Fresh f) = Fresh $ \_ i -> f str i


labelInt :: Fresh String
labelInt = Fresh (\s i -> (i, s ++ show i))

labelNoInt = Fresh $ \s i -> (i, s)

labelAppendInt :: Fresh a -> Fresh a
labelAppendInt (Fresh f) = Fresh $ \s i -> f (s ++ show i) i

labelNewCount :: Fresh String
labelNewCount = Fresh $ \s i -> (1, s ++ ": 0")

testFresh :: GMCode -> Fresh GMCode
testFresh [] = return []
testFresh (x:xs) = if x == PushGlobal "test"
                   then do
                            a <- fresh
                            ys <- testFresh xs
                            return (PushGlobal a: ys)
                   else do
                            ys <- testFresh xs
                            return (x : ys)

labelSC :: GMCode -> Fresh CodeTree
labelSC     [] = return Nil
labelSC (xs) = do
                top <- scfresh
                ys <- labelCode xs
                return $ (Code (FunDef top) `Append` ys) --`Append` (Code $ Label $ top ++ ":End")

labelCode :: GMCode -> Fresh CodeTree
labelCode     [] = return Nil
labelCode (x:xs) =
    case x of
        Casejump alts -> do 
                    startCase <- labelInt
                    alts' <- labelAppendInt $ labelCases alts
                    ys    <- labelCode xs
                    return $ (Code (CasejumpInstr startCase) `Append` alts') 
                             `Append` (Code $ Label $ startCase ++ ":EndCase")
                             `Append` ys
        otherwise     -> do
                    ys    <- labelCode xs
                    return (Code x `Append` ys)
{-
labelCasejump :: GMCode -> FreshCodeTree
labelCasejump 
-}

labelCases :: [(Int, GMCode)] -> Fresh CodeTree
labelCases     [] = return Nil
labelCases (x:xs) =
    do
        y <- labelCaseAlt x
        ys <- labelCases xs
        return (y `Append` ys)

labelCaseAlt :: (Int, GMCode) -> Fresh CodeTree
labelCaseAlt (tag, code) = do
    altLabel <- labelNoInt
    freshCode <- labelCode code
    return ( Code (CaseAlt (altLabel ++ ": " ++ show tag)) `Append` freshCode
                            `Append` Code (CaseAltEnd $ altLabel))

labelProgram :: [GMCompiledSC] -> GMCode
labelProgram []     = []
labelProgram ((name, arity, code):xs) = 
    (flattenCode $ snd (runFresh (labelSC code) name arity)) ++ labelProgram xs

--tester :: Fresh [GMCode] -> Fresh GMCode
--tester xs = return (concat xs)

type GMCompiledSC = (Name, Int, GMCode)

--This function compiles to a list of tuples (one for each supercombinator).
--Each tuple consists of the name of the SC, its arity, and the Code for the
--supercombinator
compile :: CoreProgram -> [GMCompiledSC]
compile prog = map compileSC (prog ++ preludeDefs)
                             ++ compiledPrimitives


--This is the top-level compile function, it creates a heap with all of the
--global function instances
compileToHeap :: CoreProgram -> (GMHeap, GMGlobals)
compileToHeap prog = addr `seq` (heap, globals)
        where (heap, globals) = buildInitialHeap prog
              addr            = aLookupString globals "main" 
                                              (error "Main undefined")


buildInitialHeap :: CoreProgram -> (GMHeap, GMGlobals)
buildInitialHeap prog = 
        mapAccuml allocateSC hInitial compiled
            where compiled = map compileSC (prog ++ preludeDefs)
                             ++ compiledPrimitives

--allocating SCs in the heap makes sure that there is a new heap containing the
--new global representing the SC
allocateSC :: GMHeap -> GMCompiledSC -> (GMHeap, (Name, Addr))
allocateSC heap (name, numArgs, gmCode) = (newHeap, (name, addr))
        where
            (newHeap, addr) = hAlloc heap (NGlobal numArgs gmCode)


{-Below is the section that compiles the coreExpr to GCode
 - ---------------------------------------------------------------------
 - ---------------------------------------------------------------------
 - -}
compileSC :: (Name, [Name], CoreExpr) -> GMCompiledSC
compileSC (name, env, body)
    = (name, length env, compileR body (zip env [0..]))

compileR :: GMCompiler
compileR expr env = compileE expr env ++ [Update d, Pop d, Unwind]
    where d = length env

--The compileE scheme is for the expressions that inherit a strict context from
--where they are called. 
compileE :: GMCompiler
compileE (ENum n) env = [PushInt n]
compileE (ELet recursive defs e) args
    | recursive             = compileLetRec compileE defs e args
    | otherwise             = compileLet    compileE defs e args
compileE exp@(EVar arith `EAp` e1 `EAp` e2) env
    | elem arith (aDomain builtInDyadic) = compileE e2 env ++
                                           compileE e1 (argOffset 1 env) ++
                                           [aLookupString builtInDyadic arith (error "Can't happen")]
compileE (EVar "negate" `EAp` e1) env    = compileE e1 env ++ [Neg]
--compileE (EVar "if" `EAp` e0 `EAp` e1 `EAp` e2) env =
--                          compileE e0 env ++ [Cond (compileE e1 env) (compileE e2 env)]
compileE (ECase sub alts) env            
    = compileE sub env ++ [Casejump $ compileAlts compileE' alts env]
compileE (EConstrAp tag arity args) env    
    = compilePack (reverse args) env ++ [Pack tag arity]
compileE expr env                        = compileC expr env ++ [Eval]


{-This was my original idea for compiling constructors, now I think it's bad and
 - that there is no difference between constructors and should treat it as EAps

-}

compilePack :: [CoreExpr] -> GMEnvironment -> [Instruction]
compilePack []     env = []
compilePack (a:as) env
    = compileC a env ++ compilePack as (argOffset 1 env)
--The compileE' scheme is used to wrap a Split and Slide instruction around the
--results of the normal compileE scheme, this is used when compiling case
--expressions. 
compileE' :: Int -> GMCompiler
compileE' offset expr env
    = [Split offset] ++ compileE expr env ++ [Slide offset]

--CompileAlts corresponds to the compileD scheme in the IFL book. The arguments
--this function takes will be described below.
compileAlts :: (Int -> GMCompiler) --compiler for alternates
               -> [CoreAlt]        --the list of alternates
               -> GMEnvironment    --the current environment
               -> [(Int, GMCode)]  --the output list of alternats sequences
compileAlts comp alts env
    = [(tag, 
      comp (length names) body (zip names [0..] ++ argOffset (length names) env))
        | (tag, names, body) <- alts]

builtInDyadic :: Assoc Name Instruction
builtInDyadic
    = [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), 
       ("==", Eq), ("~=", Ne), (">=", Ge), ("div", Div),
       (">", Gt), ("<=", Le), ("<", Lt)] 


compileC :: GMCompiler
compileC (EVar v) env
    | elem v (aDomain env)  = [Push n]
    | otherwise             = [PushGlobal v]
    where n = aLookupString env v (error "This error can't possibly happen")
compileC (ENum n) env       = [PushInt n]
compileC (EAp e1 e2) env    = compileC e2 env ++ 
                              compileC e1 (argOffset 1 env) ++ 
                              [MkAp]
compileC (EConstrAp tag arity args) env = compilePack (reverse args) env ++ [Pack tag arity]
compileC (ELet recursive defs e) args
    | recursive             = compileLetRec compileC defs e args
    | otherwise             = compileLet    compileC defs e args

compileLet :: GMCompiler -> [(Name, CoreExpr)] -> GMCompiler
compileLet comp defs expr env
    = compileLet' defs env ++ comp expr env' ++ [Slide (length defs)]
      where env' = compileArgs defs env

compileLet' :: [(Name, CoreExpr)] -> GMEnvironment -> GMCode
compileLet' []                  env = []
compileLet' ((name, expr):defs) env 
    = compileC expr env ++ compileLet' defs (argOffset 1 env)

compileLetRec :: GMCompiler -> [(Name, CoreExpr)] -> GMCompiler
compileLetRec comp defs expr env
    = [Alloc n] ++ compileLetRec' defs env' ++ comp expr env' ++ [Slide n]
        where 
            n    = length defs
            env' = compileArgs defs env

compileLetRec' :: [(Name, CoreExpr)] -> GMEnvironment -> GMCode
compileLetRec' []   env = []
compileLetRec' defs env
    = compileC expr env ++ [Update (n-1)] ++ compileLetRec' rest (argOffset 1 env)
    where
        ((name, expr):rest) = defs
        n                   = length defs

compileArgs :: [(Name, CoreExpr)] -> GMEnvironment -> GMEnvironment
compileArgs defs env
    = zip (map fst defs) [n-1, n-2 .. 0] ++ argOffset n env
        where n = length defs

type GMCompiler = CoreExpr -> GMEnvironment -> GMCode

type GMEnvironment = Assoc Name Int

argOffset :: Int -> GMEnvironment -> GMEnvironment
argOffset n env = [(v, n+m) | (v, m) <- env]

--In the Mark I GMachine there are no primitives, but we should still have them
--defined
compiledPrimitives :: [GMCompiledSC]
compiledPrimitives 
    = [("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind])
      ,("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind])
      ,("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind])
      ,("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind])
      ,("negate", 2, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind])
      ,("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind])
      ,("/=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind])
      ,("<",  2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind])
      ,("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind])
      ,(">",  2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind])
      ,(">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind])
      ,("parSPJ", 2, [Push 1, Push 1, MkAp, Push 2, Par, Update 2, Pop 2, Unwind])
      ,("parSPJOff", 2, [Push 1, Push 1, MkAp, Update 2, Pop 2, Unwind])
      ,("par", 2, [Push 1, Push 1, Par, Update 2, Pop 2, Unwind]) --I Think this is right.
      ,("parSPJOff", 2, [Push 1, Update 2, Pop 2, Unwind])]
--      ,("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])]
{-
labelSuperComb ::  GMHeap -> (Name, Addr) -> Fresh String
labelSuperComb heap (name, addr) = runFresh labelCode name 0
-}
{-The following are the printing functions needed for when viewing the results
 - of compilation in GHCI
 -}
showCode :: (GMHeap, GMGlobals) -> String
showCode (heap, globals)
    = iDisplay $ iConcat [IStr "Supercombinator definitions:", INewline
                         ,iInterleave INewline (map (showSC heap) globals)
                         ,INewline, INewline]

showSC :: GMHeap -> (Name, Addr) -> Iseq
showSC heap (name, addr)
    = iConcat [ IStr "Code for ", IStr name, INewline
               ,showInstructions code, INewline, INewline]
        where 
            (NGlobal arity code) = (hLookup heap addr)

showInstructions :: GMCode -> Iseq
showInstructions code 
    = iConcat [IStr "  Code:{",
               IIndent (iInterleave INewline (map showInstruction code)),
               IStr "}", INewline]

--For when printing the instructions to a file
printInstructions :: GMCode -> Iseq
printInstructions code 
    = iInterleave INewline (map showInstruction code)

--Functions to turn each instruction into an appropriate Iseq
showInstruction :: Instruction -> Iseq
showInstruction Unwind         = IStr "Unwind"
showInstruction (PushGlobal f) = (IStr "PushGlobal ") `IAppend` (IStr f)
showInstruction (Push n)       = (IStr "Push ") `IAppend` (iNum n)
showInstruction (PushInt n)    = (IStr "PushInt ") `IAppend` (iNum n)   
showInstruction MkAp           = IStr "MkAp"
showInstruction (Update n)     = (IStr "Update ") `IAppend` (iNum n)   
showInstruction (Pop n)        = (IStr "Pop ") `IAppend` (iNum n)   
showInstruction (Slide n)      = (IStr "Slide ") `IAppend` (iNum n)   
showInstruction (Alloc n)      = (IStr "Alloc ") `IAppend` (iNum n)   
showInstruction Eval           = IStr "Eval"
showInstruction Add            = IStr "Add"
showInstruction Sub            = IStr "Sub"
showInstruction Mul            = IStr "Mul"
showInstruction Div            = IStr "Div"
showInstruction Neg            = IStr "Neg"
showInstruction Eq             = IStr "Eq"
showInstruction Ne             = IStr "Ne"
showInstruction Lt             = IStr "Lt"
showInstruction Le             = IStr "Le"
showInstruction Gt             = IStr "Gt"
showInstruction Ge             = IStr "Ge"
showInstruction (Pack n1 n2)   = iConcat [IStr "Pack ", iNum n1, IStr " "
                                         ,iNum n2]
showInstruction (Casejump as)  = iConcat ([IStr "Casejump: ", INewline] 
                                         ++ map showCasejump as)
showInstruction (CasejumpInstr s) = IStr $ "CaseJump: " ++ s
showInstruction (Split n)      = iConcat [IStr "Split ", iNum n]
showInstruction Print          = IStr "Print"
showInstruction (Cond a1 a2)   = iConcat 
                                    [IStr "Cond: ", INewline, IStr "Alt1: ",
                                    (iInterleave INewline (map showInstruction a1)),
                                    INewline, IStr "Alt2: ",
                                    (iInterleave INewline (map showInstruction a2))]
showInstruction Par            = IStr "Par"
showInstruction (Label str)    = IStr ("Label: " ++ str)
showInstruction (FunDef str)    = IStr ("FunDef: " ++ str)
showInstruction Case           = IStr "Case"
showInstruction (CaseAlt str)  = IStr ("CaseAlt: " ++ str)
showInstruction (CaseAltEnd str)= IStr ("CaseAltEnd: " ++ str)


showCasejump (num, code) = iConcat [iNum num, INewline
                                   ,IIndent (iInterleave INewline 
                                            (map showInstruction code))]
