import Language
import Parser
import Heap
import Data.List

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
        | Split Int
        | Print
        | Par
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


compile :: CoreProgram -> (GMHeap, GMGlobals)
compile prog = addr `seq` (heap, globals)
        where (heap, globals) = buildInitialHeap prog
              addr            = aLookupString globals "main" 
                                              (error "Main undefined")

type GMCompiledSC = (Name, Int, GMCode)

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

--The special-case supercombinator "main" is required of every program and is
--the first function to be called. So the initial gCode for every program will
--be loading that supercombinator
initCode :: GMCode
initCode = [Eval, Print]

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
compileE (EVar "if" `EAp` e0 `EAp` e1 `EAp` e2) env =
                          compileE e0 env ++ [Cond (compileE e1 env) (compileE e2 env)]
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
      ,("par", 2, [Push 1, Push 1, MkAp, Push 2, Par, Update 2, Pop 2, Unwind])]
--      ,("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])]


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

--Functions to turn each instruction into an appropriate Iseq
showInstruction :: Instruction -> Iseq
showInstruction Unwind         = IStr "Unwind"
showInstruction (PushGlobal f) = (IStr "Pushglobal ") `IAppend` (IStr f)
showInstruction (Push n)       = (IStr "Push ") `IAppend` (iNum n)
showInstruction (PushInt n)    = (IStr "Pushint ") `IAppend` (iNum n)   
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
showInstruction (Pack n1 n2)   = iConcat [IStr "Pack{", iNum n1, IStr ","
                                         ,iNum n2, IStr "}"]
showInstruction (Casejump as)  = iConcat ([IStr "Casejump: ", INewline] 
                                         ++ map showCasejump as)
showInstruction (Split n)      = iConcat [IStr "Split ", iNum n]
showInstruction Print          = IStr "Print"
showInstruction (Cond a1 a2)   = iConcat 
                                    [IStr "Cond: ", INewline, IStr "Alt1: ",
                                    (iInterleave INewline (map showInstruction a1)),
                                    INewline, IStr "Alt2: ",
                                    (iInterleave INewline (map showInstruction a2))]
showInstruction Par            = IStr "Par"


showCasejump (num, code) = iConcat [iNum num, INewline
                                   ,IIndent (iInterleave INewline 
                                            (map showInstruction code))]
