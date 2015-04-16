module Compiler where

import Language
import Parser
import Heap
import LambdaLift
import CaseLift
import Fresh
import Data.List

compileToGCode = labelProgram . compile . caseLift . parse

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
    deriving (Eq, Show)


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
        deriving Show
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


{- ensure that all casejumps have unique lables -}
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
                    alts' <- labelAppendInt $ labelCases alts startCase
                    ys    <- labelCode xs
                    return $ (Code (CasejumpInstr startCase) `Append` alts') 
                             `Append` (Code $ Label $ startCase ++ ":EndCase")
                             `Append` ys
        otherwise     -> do
                    ys    <- labelCode xs
                    return (Code x `Append` ys)

labelCases :: [(Int, GMCode)] -> String -> Fresh CodeTree
labelCases     [] l = return Nil
labelCases (x:xs) l =
    do
        y <- labelCaseAlt x l
        ys <- labelCases xs l
        return (y `Append` ys)

labelCaseAlt :: (Int, GMCode) -> String -> Fresh CodeTree
labelCaseAlt (tag, code) l = do
    freshCode <- labelCode code
    return ( Code (CaseAlt (l ++ ": " ++ show tag)) `Append` freshCode
                            `Append` Code (CaseAltEnd $ l))

labelProgram :: [GMCompiledSC] -> GMCode
labelProgram []     = []
labelProgram ((name, arity, code):xs) = 
    (flattenCode $ snd (runFresh (labelSC code) name arity)) ++ labelProgram xs

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
    = (name, d, compileR body (zip env [0..]) d)
  where
    d = length env

compileR :: GMCompiler
compileR (ELet recursive defs e) env d
    | recursive = compileLetRec True compileR defs e env d
    | otherwise = compileLet True compileR defs e env d
compileR e@(EAp e1 e2)           env d = compileRT e env 0
compileR (ECase sub alts) env d
    = compileE sub env d ++ [Casejump $ compileAlts (compileE' True) alts env d]
compileR expr env d = compileE expr env d ++ [Update d, Pop d, Unwind]

compileRT :: GMCompiler
compileRT (EVar v) env d
    | elem v (aDomain env)           = [Push n] ++ (take d $ repeat MkAp) ++ [Update d', Pop d', Unwind]
    | elem v (aDomain builtInDyadic) = [aLookupString builtInDyadic v (error "Can't happen")] ++ [Update d', Pop d', Unwind]
    | otherwise                      = [PushGlobal v] ++ (take d $ repeat MkAp) ++ [Update d', Pop d', Unwind]
        where
            n  = aLookupString env v (error "This error can't possibly happen")
            d' = length env
compileRT (EVar arith `EAp` e1 `EAp` e2) env d
    | elem arith (aDomain builtInDyadic) = compileE e2 env d ++
                                           compileE e1 (argOffset 1 env) (d + 1) ++
                                           compileRT (EVar arith) (argOffset 2 env) (d + 2)
                                           -- [aLookupString builtInDyadic arith (error "Can't happen")]
compileRT (EVar "negate" `EAp` e1) env d  = compileE e1 env d ++ [Neg] ++ [Update d', Pop d', Unwind]
  where
    d' = length env
compileRT (EAp e1 e2) env d = compileC e2 env d ++ compileRT e1 (argOffset 1 env) (d + 1)

--The compileE scheme is for the expressions that inherit a strict context from
--where they are called. 
compileE :: GMCompiler
compileE (ENum n) env d = [PushInt n]
compileE (ELet recursive defs e) args d
    | recursive             = compileLetRec False compileE defs e args d
    | otherwise             = compileLet False compileE defs e args d
compileE (EVar arith `EAp` e1 `EAp` e2) env d
    | elem arith (aDomain builtInDyadic) = compileE e2 env d ++
                                           compileE e1 (argOffset 1 env) (d + 1) ++
                                           [aLookupString builtInDyadic arith (error "Can't happen")]
compileE (EVar "negate" `EAp` e1) env d  = compileE e1 env d ++ [Neg]
compileE (ECase sub alts) env d
    = compileE sub env d ++ [Casejump $ compileAlts (compileE' False) alts env d]
compileE (EConstrAp tag arity args) env d
    = compilePack (reverse args) env d ++ [Pack tag arity]
compileE expr env d
    = compileC expr env d ++ [Eval]


{-This was my original idea for compiling constructors, now I think it's bad and
 - that there is no difference between constructors and should treat it as EAps

-}

compilePack :: [CoreExpr] -> GMEnvironment -> Int -> [Instruction]
compilePack []     _    _ = []
compilePack (a:as) env  d
    = compileC a env d ++ compilePack as (argOffset 1 env) (d + 1)

--The compileE' scheme is used to wrap a Split and Slide instruction around the
--results of the normal compileE scheme, this is used when compiling case
--expressions. 
compileE' :: Bool -> Int -> GMCompiler
compileE' fromR offset expr env d 
    | fromR     = [Split offset] ++ compileR expr env d
    | otherwise = [Split offset] ++ compileE expr env d ++ [Slide offset]

--CompileAlts corresponds to the compileD scheme in the IFL book. The arguments
--this function takes will be described below.
compileAlts :: (Int -> GMCompiler) --compiler for alternates
               -> [CoreAlt]        --the list of alternates
               -> GMEnvironment    --the current environment
               -> Int              --current stack depth
               -> [(Int, GMCode)]  --the output list of alternats sequences
compileAlts comp alts env d
    = [(tag, 
      comp (length names) body (zip names [0..] ++ argOffset (length names) env) (d + (length names)))
        | (tag, names, body) <- alts]

builtInDyadic :: Assoc Name Instruction
builtInDyadic
    = [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), 
       ("==", Eq), ("/=", Ne), (">=", Ge), ("div", Div),
       (">", Gt), ("<=", Le), ("<", Lt)] 


compileC :: GMCompiler
compileC (EVar v) env d
    | elem v (aDomain env)  = [Push n]
    | otherwise             = [PushGlobal v]
    where n = aLookupString env v (error "This error can't possibly happen")
compileC (ENum n) env d     = [PushInt n]
compileC (EAp e1 e2) env d  = compileC e2 env d ++ 
                              compileC e1 (argOffset 1 env) (d + 1) ++ 
                              [MkAp]
compileC (EConstrAp tag arity args) env d = compilePack (reverse args) env d ++ [Pack tag arity]
compileC (ELet recursive defs e) args d
    | recursive             = compileLetRec False compileC defs e args d
    | otherwise             = compileLet False compileC defs e args d
compileC x env d            = error $ "Compile scheme C reach unexpected expression: " ++ show x 

compileLet :: Bool -> GMCompiler -> [(Name, CoreExpr)] -> GMCompiler
compileLet fromR comp defs expr env d
    = compileLet' defs env d ++ comp expr env' (d + lds) ++ slide
      where env'  = compileArgs defs env
            lds   = length defs
            slide = if fromR then [] else [Slide lds]

compileLet' :: [(Name, CoreExpr)] -> GMEnvironment -> Int -> GMCode
compileLet' []                  env d = []
compileLet' ((name, expr):defs) env d
    = compileC expr env d ++ compileLet' defs (argOffset 1 env) (d + 1)

compileLetRec :: Bool -> GMCompiler -> [(Name, CoreExpr)] -> GMCompiler
compileLetRec fromR comp defs expr env d
    = [Alloc n] ++ compileLetRec' defs env' d ++ comp expr env' (d + n) ++ slide
        where 
            n     = length defs
            env'  = compileArgs defs env
            slide = if fromR then [] else [Slide n]

compileLetRec' :: [(Name, CoreExpr)] -> GMEnvironment -> Int -> GMCode
compileLetRec' []   env d = []
compileLetRec' defs env d
    = compileC expr env d ++ [Update (n-1)] ++ compileLetRec' rest (argOffset 1 env) (d + 1)
    where
        ((name, expr):rest) = defs
        n                   = length defs

compileArgs :: [(Name, CoreExpr)] -> GMEnvironment -> GMEnvironment
compileArgs defs env
    = zip (map fst defs) [n-1, n-2 .. 0] ++ argOffset n env
        where n = length defs

type GMCompiler = CoreExpr -> GMEnvironment -> Int -> GMCode

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
      ,("par", 2, [Push 1, Push 1, Par, Eval, Update 2, Pop 2, Unwind]) --I Think this is right.
      ,("parStrat", 2, [Push 1, Push 1, Par, Eval, Update 2, Pop 2, Unwind]) -- This is right if par is right
      ,("parOff", 2, [Push 1, Eval, Update 2, Pop 2, Unwind])
      ,("fix", 2, [Push 1, Push 1, PushGlobal "fix", MkAp, Push 2, MkAp, MkAp, Eval, Update 2, Pop 2, Unwind])
      ,("seq", 2, [Push 1, Push 1, Eval, Pop 1, Eval, Update 2, Pop 2, Unwind]) --I Think this is right.
      ]
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
