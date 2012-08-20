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

type PGMGlobalState = (GMOutput,
                       GMHeap,
                       GMGlobals,
                       GMSparks,
                       PGMStats)

--The local state for each thread in the parallel machine will include all of
--the data that is essential for graph reduction, but none of the data that is
--shared between threads
type PGMLocalState = (GMCode,
                      GMStack,
                      GMDump,
                      GMClock)

--We also need a w structure for holding a thread's local state AND the global
--state
type GMState = (PGMGlobalState, PGMLocalState)


--GMOutput is for the currently computed output of the program. This can be used
--to recursively find the final output
type GMOutput = [Char]

getOutput :: GMState -> GMOutput
getOutput ((o, heap, globals, sparks, stats), locals) = o

putOutput :: GMOutput -> GMState -> GMState
putOutput o' ((o, heap, globals, sparks, stats), locals)
    = ((o', heap, globals, sparks, stats), locals)

getGlobalOut ::PGMState -> GMOutput
getGlobalOut ((o, heap, globals, sparks, stats), locals) = o

--The types for the Dump and the items in the Dump are as follows
type GMDump = [GMDumpItem]
type GMDumpItem = (GMCode, GMStack)

--'Setter and getter' for GMDump
getDump :: GMState -> GMDump
getDump (globals, (code, stack, dump, clock)) = dump

putDump :: GMDump -> GMState -> GMState
putDump dump' (globals, (code, stack, dump, clock))
    = (globals, (code, stack, dump', clock))

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

getCode :: GMState -> GMCode
getCode (globals, (code, stack, dump, clock)) = code

putCode :: GMCode -> GMState -> GMState
putCode code' (globals, (code, stack, dump, clock)) 
    = (globals, (code', stack, dump, clock)) 


--The code for the GMStack is below:
type GMStack = [Addr]

getStack :: GMState -> GMStack
getStack (globals, (code, stack, dump, clock)) = stack

putStack :: GMStack -> GMState -> GMState
putStack stack' (globals, (code, stack, dump, clock))
    = (globals, (code, stack', dump, clock))

--The code for the state's heap:
type GMHeap = Heap Node

--A Node is either a number (a result), an application of two other nodes, or
--the arity & gCode sequence for when the global can be executed
data Node = 
          NNum Int
        | NAp Addr Addr
        | NGlobal Int GMCode
        | NInd Addr                           --Indirection node
        | NConstr Int [Addr]
        | NLAp Addr Addr PGMPendingList       --Locked Application node
        | NLGlobal Int GMCode PGMPendingList  --Locked global
    deriving Eq

type PGMPendingList = [PGMLocalState]

getHeap :: GMState -> GMHeap
getHeap ((o, heap, globals, sparks, stats), locals) = heap

putHeap :: GMHeap -> GMState -> GMState
putHeap h' ((o, heap, globals, sparks, stats), locals)
    = ((o, h', globals, sparks, stats), locals)

getGlobalHeap :: PGMState -> GMHeap
getGlobalHeap ((o, heap, globals, sparks, stats), locals) = heap

--The code for GMGlobals is below. Because the globals of a program do not
--change during execution, a `put' function is not needed
type GMGlobals = Assoc Name Addr

getGlobals :: GMState -> GMGlobals
getGlobals ((o, heap, globals, sparks, stats), locals) = globals

putGlobals :: (Name, Addr) -> GMState -> GMState
putGlobals global ((o, heap, globals, sparks, stats), locals)
    = ((o, heap, global:globals, sparks, stats), locals)

getGlobGlobals :: PGMState -> GMGlobals
getGlobGlobals ((o, heap, globals, sparks, stats), locals) = globals

--For the parallel machine we need a way to store sparked threads. These will be
--pointers into the heap which can then be picked up and evaluated.
type GMSparks = [PGMLocalState]

getSparks :: GMState -> GMSparks
getSparks ((o, heap, globals, sparks, stats), locals) = sparks

getPGMSparks :: PGMState -> GMSparks
getPGMSparks ((o, heap, globals, sparks, stats), locals) = sparks

putSparks :: GMSparks -> GMState -> GMState
putSparks sparks' ((o, heap, globals, sparks, stats), locals)
    = ((o, heap, globals, sparks', stats), locals)

putPGMSparks :: GMSparks -> PGMState -> PGMState
putPGMSparks sparks' ((o, heap, globals, sparks, stats), locals)
    = ((o, heap, globals, sparks', stats), locals)

--GMStats:
type GMStats = Int

statInitial :: GMStats
statInitial = 0

statIncSteps :: GMStats -> GMStats
statIncSteps s = s + 1

statGetSteps :: GMClock -> Int
statGetSteps s = s

--PGMStats will hold the number of blocked, running, and idle tasks for each
--clock tick of the evaluator. The list of Ints satisfies accomodates the old
--use of PGMStats by taking the number of clock ticks for dying tasks
type PGMStats = ((BlockedTasks, RunningTasks, IdleTasks), [Int])
type BlockedTasks = Int
type RunningTasks = Int
type IdleTasks = Int

getStats :: GMState -> PGMStats
getStats ((o, heap, globals, sparks, stats), locals) = stats

getPGMStats :: PGMState -> PGMStats
getPGMStats ((o, heap, globals, sparks, stats), locals) = stats

putStats :: PGMStats -> GMState -> GMState
putStats stats' ((o, heap, globals, sparks, stats), locals)
    = ((o, heap, globals, sparks, stats'), locals)

putPGMStats :: PGMStats -> PGMState -> PGMState
putPGMStats stats' ((o, heap, globals, sparks, stats), locals)
    = ((o, heap, globals, sparks, stats'), locals)

nBlocked :: Int -> GMState -> GMState
nBlocked n state@((o, h, globs, sparks, ((blocked, running, idle), count)), locals)
    = putStats ((blocked+n, running, idle), count) state

nFreed :: Int -> GMState -> GMState
nFreed n state@((o, h, globs, sparks, ((blocked, running, idle), count)), locals)
    = putStats ((blocked-n, running, idle+n), count) state

nRunning :: PGMState -> PGMState
nRunning state@((o, h, globs, sparks, ((blocked, running, idle), count)), locals)
    = putPGMStats ((blocked, n, idle), count) state
        where n = length locals

nIdle :: PGMState -> PGMState
nIdle state@((o, h, globs, sparks, ((blocked, running, idle), count)), locals)
    = putPGMStats ((blocked, running, n), count) state
        where n = length sparks

getNumBlocked :: PGMState -> Int
getNumBlocked ((o, h, glbs, sprks, ((blocked, running, idle), count)), locals)
    = blocked
 

sumStats :: PGMState -> Int
sumStats state
    = sum $ snd $ getPGMStats state

maxStat :: PGMState -> Int
maxStat state
    = maximum' $ snd $ getPGMStats state

--The GMClock keeps track of the number of steps for an individual thread
type GMClock = Int

getClock :: GMState -> GMClock
getClock (globals, (code, stack, dump, clock)) = clock

putClock :: GMClock -> GMState -> GMState
putClock clock' (globals, (code, stack, dump, clock))
    = (globals, (code, stack, dump, clock'))

--Similarly, for the parallel machine we have the following state tuple
type PGMState = (PGMGlobalState, --State shared by all threads
                 [PGMLocalState])--State for each thread


--InitialTask takes an address and forms a LocalState structure
initialTask :: Addr -> PGMLocalState
initialTask addr = (initCode, [addr], [], 0)

compile :: CoreProgram -> PGMState
compile prog = (([], heap, globals, [], ((0,1,0),[])), [initialTask addr])
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
showResults :: [PGMState] -> String
showResults states
    = iDisplay $ iConcat [IStr "Supercombinator definitions:", INewline
                         ,iInterleave INewline (map (showSC s) (getGlobGlobals s))
                         ,INewline, INewline, IStr "State transitions:", INewline
                         ,INewline
                         ,iLayn (map showState states), INewline, INewline
                         ,showStats states]
                where (s:ss) = states

--showResult is for when we only want to see the result of the computation and
--not the intermediate steps
showResult :: PGMState -> String
showResult state
    = iDisplay (iConcat [IStr "Output Register: ", IStr (getGlobalOut state)
                        ,INewline, showStat state, INewline])

showSC :: PGMState -> (Name, Addr) -> Iseq
showSC state (name, addr)
    = iConcat [ IStr "Code for ", IStr name, INewline
               ,showInstructions code, INewline, INewline]
        where 
            (NGlobal arity code) = (hLookup (getGlobalHeap state) addr)

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

--showState will take the GCode and the stack from a given state and 
--wrap them in Iseqs
showState :: PGMState -> Iseq
showState state
    = iConcat ([showOutput state, INewline] ++
              [showSparks state, INewline] ++
              [showBlocked state, INewline] ++
              showState' states)
      where (global, locals) = state
            states           = [(global, a) | a <- locals]

showState' :: [GMState] -> [Iseq]
showState' states
    = concat [[showStack (global, a), INewline,
              showDump (global, a),  INewline,
              showInstructions (getCode (global, a)), INewline] |
              a <- locals]
      where locals = [a | a <- map snd states]
            global = fst $ head states



showStat :: PGMState -> Iseq
showStat state = iConcat [IStr "Steps taken: ", iNum (sumStats state)
                          ,INewline, IStr "Max Thread length: "
                          ,iNum (maxStat state), INewline]

showStats :: [PGMState] -> Iseq
showStats state = iConcat [IStr "Steps taken: ", iNum (sumStats $ last state)
                          ,INewline, IStr "Max Thread length: "
                          ,iNum (maxStat $ last state), INewline
                          ,IStr "Max simultaneous threads: "
                          ,iNum $ maximum $ map (length.snd) state, INewline]



--showOutput is easy as its component is already a string
showOutput :: PGMState -> Iseq
showOutput state = iConcat [IStr " Output:\""
                           ,IStr (getOutput state')
                           ,IStr "\""]
                    where state' = (fst state, head $ snd state)

showSparks :: PGMState -> Iseq
showSparks state = iConcat [IStr " Number of Sparks: "
                           ,iNum $ length (getPGMSparks state)]

showBlocked :: PGMState -> Iseq
showBlocked state = iConcat [IStr " Number of Blocked Tasks: "
                            ,iNum $ getNumBlocked state]

--showStacks takes the entire PGMState and creates a list of GMStates 
--which showStack is then mapped over
showStacks :: [GMState] -> Iseq
showStacks states
    = iConcat $ map showStack states

--When preparing the stack to be printed
showStack :: GMState -> Iseq
showStack state
    = iConcat [IStr " Stack:["
              ,IIndent (iInterleave INewline
                                    (map (showStackItem state)
                                         (reverse (getStack state))))
              ,IStr "]"]

showStackItem :: GMState -> Addr -> Iseq
showStackItem state addr
    = iConcat [IStr (showAddr addr), IStr ": "
              ,showNode state addr (hLookup (getHeap state) addr)]

--showDumps is to dumps as showStacks is to stacks
showDumps :: [GMState] -> Iseq
showDumps states
    = iConcat $ map showDump states

showDump :: GMState -> Iseq
showDump state = iConcat [IStr " Dump:["
                         ,IIndent (iInterleave INewline
                                  (map showDumpItem (reverse (getDump state))))
                         ,IStr "]"]

showDumpItem :: GMDumpItem -> Iseq
showDumpItem (code, stack) 
    = iConcat [IStr "<"
              ,shortShowInstructions 3 code, IStr ", "  
              ,shortShowStack stack, IStr ">"]


showNode :: GMState -> Addr -> Node -> Iseq
showNode state addr (NNum n)         = iNum n 
showNode state addr (NAp ad1 ad2)    = iConcat [IStr "Ap ", IStr (showAddr ad1)
                                               ,IStr " ", IStr (showAddr ad2)]
showNode state addr (NGlobal n code) = iConcat [IStr "Global ", IStr v]
                    where v = head [n | (n, b) <- getGlobals state, addr == b]
showNode state addr (NInd ad1)  = iConcat [IStr "Indirection "
                                          ,IStr (showAddr ad1)]
showNode state addr (NConstr t as) = 
        iConcat [IStr "Constr ", iNum t, IStr " ["
                ,iInterleave (IStr ", ") (map (IStr . showAddr) as)
                ,IStr "]"]
showNode state addr (NLAp ad1 ad2 blocked)    = iConcat [IStr "Locked Ap ", IStr (showAddr ad1)
                                               ,IStr " ", IStr (showAddr ad2)
                                               ,IStr " blocked nodes: ", iNum $ length blocked]
showNode state addr (NLGlobal n code blocked) = iConcat [IStr "Locked Global ", IStr v
                                               ,IStr " blocked nodes: ", iNum $ length blocked]
                    where v = head [n | (n, b) <- getGlobals state, addr == b]


shortShowStack :: GMStack -> Iseq
shortShowStack stack
    = iConcat [IStr "["
              ,iInterleave (IStr ", ") (map (IStr . showAddr) stack)
              ,IStr "]"]


shortShowInstructions :: Int -> GMCode -> Iseq
shortShowInstructions num code
    = iConcat [IStr "{", iInterleave (IStr "; ") dotcodes, IStr "}"]
    where
        codes = map showInstruction (take num code)
        dotcodes 
            | length code > num = codes ++ [IStr "..."]
            | otherwise         = codes
