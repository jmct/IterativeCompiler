Silly change
{-This file will contain all the code necessary for the basic GMachine
 -
 - the functions defined here or called from {Language, Parser, Heap}.hs
 - will enable us to define and call the following top-level function for
 - running programs:
 -
 - runProg :: [Char] -> [Char]
 - runProg = showResults . eval . compile . parse
 -}
import Language
import Parser
import Heap

--The state of the G-Machine will be stored in the following 5-tuple
type GMState = (GMCode,     --Current instuction stream
                GMStack,    --Current Stack
                GMDump,     --Stack dump for saving machine context
                GMHeap,     --The Heap holding the program graph
                GMGlobals,  --Addresses for globals in the heap
                GMStats)    --Statistics about the computation

--The types for the Dump and the items in the Dump are as follows
type GMDump = [GMDumpItem]
type GMDumpItem = (GMCode, GMStack)

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
        | Eq | Ne | Lt | Gt | Ge        --comparison instructions
        | Cond GMCode GMCode            --conditional with alternatives
    deriving Eq

getCode :: GMState -> GMCode
getCode (code, _, _, _, _, _) = code

putCode :: GMCode -> GMState -> GMState
putCode code' (code, stack, dump, heap, globals, stats) 
    = (code', stack, dump, heap, globals, stats)


--The code for the GMStack is below:
type GMStack = [Addr]

getStack :: GMState -> GMStack
getStack (_, stack, _, _, _, _) = stack

putStack :: GMStack -> GMState -> GMState
putStack stack' (code, stack, dump, heap, globals, stats) 
    = (code, stack', dump, heap, globals, stats)

--'Setter and getter' for GMDump
getDump :: GMState -> GMDump
getDump (_, _, dump, _, _, _) = dump

putDump :: GMDump -> GMState -> GMState
putDump dump' (code, stack, dump, heap, globals, stats) 
        = (code, stack, dump', heap, globals, stats) 

--The code for the state's heap:
type GMHeap = Heap Node

--A Node is either a number (a result), an application of two other nodes, or
--the arity & gCode sequence for when the global can be executed
data Node = 
          NNum Int
        | NAp Addr Addr
        | NGlobal Int GMCode
        | NInd Addr         --Indirection node
    deriving Eq


getHeap :: GMState -> GMHeap
getHeap (_, _, _, heap, _, _) = heap

putHeap :: GMHeap -> GMState -> GMState
putHeap heap' (code, stack, dump, heap, globals, stats) 
    = (code, stack, dump, heap', globals, stats)


--The code for GMGlobals is below. Because the globals of a program do not
--change during execution, a `put' function is not needed
type GMGlobals = Assoc Name Addr

getGlobals :: GMState -> GMGlobals
getGlobals (_, _, _, _, globals, _) = globals

putGlobals :: (Name, Addr) -> GMState -> GMState
putGlobals global' (c, s, dump, h, globals, stats) 
        =  (c, s, dump, h, global':globals, stats)

--GMStats:
type GMStats = Int

statInitial :: GMStats
statInitial = 0

statIncSteps :: GMStats -> GMStats
statIncSteps s = s + 1

statGetSteps :: GMStats -> Int
statGetSteps s = s

getStats :: GMState -> GMStats
getStats (code, stack, dump, heap, globals, stats) = stats

putStats :: GMStats -> GMState -> GMState
putStats stats' (code, stack, dump, heap, globals, stats) = 
        (code, stack, dump, heap, globals, stats')

--The Evaluator function takes a list of states (the first of which is created 
--by the compiler)
eval :: GMState -> [GMState]
eval state = state : restStates
        where
        restStates 
            | gmFinal state     = []
            | otherwise         = eval nextState
        nextState = doAdmin (step state)

--doAdmin allows for any between-state calculations that need to be made. In
--this case we increment the stats counter. 
doAdmin :: GMState -> GMState
doAdmin state = putStats (statIncSteps (getStats state)) state

--gmFinal state checks if there is any code left to execute; if there is not,
--we have reached the final state. 
gmFinal :: GMState -> Bool
gmFinal s = case (getCode s) of
                []        -> True
                otherwise -> False

step :: GMState -> GMState
step state = dispatch code (putCode rest state)
            where (code:rest) = getCode state

dispatch :: Instruction -> GMState -> GMState
dispatch (PushGlobal f) = pushglobal f
dispatch (PushInt n)    = pushint n
dispatch MkAp           = mkap
dispatch (Push n)       = push n
dispatch (Slide n)      = slide n
dispatch (Pop n)        = pop n
dispatch (Update n)     = update n
dispatch Unwind         = unwind
dispatch (Alloc n)      = alloc n

pushglobal :: Name -> GMState ->GMState
pushglobal f state 
    = putStack (a : getStack state) state
        where 
        a = aLookupString (getGlobals state) f (error ("Undeclared global " ++ f))

pushint :: Int -> GMState -> GMState
pushint n state = putStack (a : getStack state') state'
    where (a, state') = case lookupRes of  
                        Just ad -> (ad, state)
                        Nothing -> cleanUp (hAlloc (getHeap state) (NNum n)) 
          lookupRes = lookup (show n) (getGlobals state)
          cleanUp (heap', ad1) = (ad1, (putHeap heap' (putGlobals (show n, ad1) state)))

mkap :: GMState -> GMState
mkap state
    = putHeap heap' (putStack (a:as') state)
    where (heap', a) = hAlloc (getHeap state) (NAp a1 a2)
          (a1:a2:as') = getStack state

push :: Int -> GMState -> GMState
push n state
    = putStack (a:as) state
    where as = getStack state
          a  = (as !! n)


slide :: Int -> GMState -> GMState
slide n state
    = putStack (a: drop n as) state
    where (a:as) = getStack state

--This is my implementation of alloc, the book suggests another route that I've
--written below
alloc :: Int -> GMState -> GMState
alloc n state
    = putHeap heap' (putStack stack' state)
        where
            hNills      = take n (repeat (NInd hNull))
            (heap', as) = mapAccuml hAlloc (getHeap state) hNills
            stack' = as ++ (getStack state)
{-The SPJ definition of alloc uses the following function in place of my use of
 - mapAccuml:
 -
 - allocNodes :: Int -> GMHeap -> (GMHeap, [Addr])
 - allocNodes 0 heap = (heap, [])
 - allocNodes n heap = (heap2, a:as)
 -      where
 -          (heap1, as) = allocNodes (n-1) heap
 -          (heap2, a)  = hAlloc heap1 (NInd hNull)
 -}


update :: Int -> GMState -> GMState
update n state
    = putStack (stack') (putHeap heap' state)
        where 
            (a:as)      = getStack state
            stack'      = drop 1 (a:as)
            an          = head (drop n stack')
            heap'  = hUpdate (getHeap state) an (NInd a)

pop :: Int -> GMState -> GMState
pop n state = putStack stack' state
    where stack' = drop n (getStack state)

unwind :: GMState -> GMState
unwind state
    = newState (hLookup heap a)
    where
        (a:as) = getStack state
        heap   = getHeap state
        (dCode, dStack) = head $ getDump state
        newState (NNum n)
                | null (getDump state) = state
                | otherwise            = putCode dCode (putStack (a:dStack) state)
        newState (NAp a1 a2) = putCode [Unwind] (putStack (a1:a:as) state)
        newState (NInd a1) = putCode [Unwind] (putStack (a1:as) state)
        newState (NGlobal n c)
                | length as < n     = error "Unwinding with too few args"
                | otherwise         = putStack (rearrange n (getHeap state) (a:as))
                                               (putCode c state)

--evalI is the code to execute when evaluating the Eval instruction. This is not
--to be confused with the eval function, which is the evaluator (GMachine)
--itself
evalI :: GMState -> GMState
evalI (code, (a:as), dump, _, _, _) 
    = putCode [Unwind] (putStack [a] (putDump ((code, as):dump) state))

rearrange :: Int -> GMHeap -> GMStack -> GMStack
rearrange n heap stack
    = take n stack' ++ drop n stack
        where
            stack' = map (getArg . hLookup heap) (tail stack)

getArg :: Node -> Addr
getArg (NAp a1 a2) = a2
getArg _ = error "Error in call to getArg: not an application node."


addI :: GMState -> GMState
addI state
    = putStack (a:s) (putHeap heap' state)
        where
            a1:a2:s    = getStack state
            heap'      = getHeap state
            num1       = hLookup heap a1
            num2       = hLookup heap a2
            (a, heap') = hAlloc heap (NNum (num1 + num2))

--mapAccuml is a utility function that will be used frequently in the code to
--come.
mapAccuml :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccuml f acc []      = (acc, [])
mapAccuml f acc (x:xs)  = (acc2, x':xs')
                where
                    (acc1, x')  = f acc x
                    (acc2, xs') = mapAccuml f acc1 xs


--Compile functions are broken into portions for compiling SC, R and C
compile :: CoreProgram -> GMState
compile prog = (initCode, [], [], heap, globals, statInitial)
        where (heap, globals) =  buildInitialHeap (prog ++ preludeDefs)

--Once compiled a supercombinator for the G-Machine will be of the form:
type GMCompiledSC = (Name, Int, GMCode)

buildInitialHeap :: CoreProgram -> (GMHeap, GMGlobals)
buildInitialHeap prog = 
        mapAccuml allocateSC hInitial compiled
            where compiled = map compileSC prog

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
initCode = [PushGlobal "main", Unwind]

compileSC :: (Name, [Name], CoreExpr) -> GMCompiledSC
compileSC (name, env, body)
    = (name, length env, compileR body (zip env [0..]))

compileR :: GMCompiler
compileR expr env = compileC expr env ++ [Update d, Pop d, Unwind]
    where d = length env

compileC :: GMCompiler
compileC (EVar v) env
    | elem v (aDomain env)  = [Push n]
    | otherwise             = [PushGlobal v]
    where n = aLookupString env v (error "This error can't possibly happen")
compileC (ENum n) env       = [PushInt n]
compileC (EAp e1 e2) env    = compileC e2 env ++ 
                              compileC e1 (argOffset 1 env) ++ 
                              [MkAp]
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
compiledPrimitives = []


{-The following are the printing functions needed for when viewing the results
 - of compilation in GHCI
 -}

showResults :: [GMState] -> String
showResults states
    = iDisplay (iConcat [IStr "Supercombinator definitions:", INewline
                        ,iInterleave INewline (map (showSC s) (getGlobals s))
                        ,INewline, INewline, IStr "State transitions:", INewline
                        ,INewline
                        ,iLayn (map showState states), INewline, INewline
                        ,showStats (last states)])
                where (s:ss) = states

showSC :: GMState -> (Name, Addr) -> Iseq
showSC state (name, addr)
    = iConcat [ IStr "Code for ", IStr name, INewline
               ,showInstructions code, INewline, INewline]
        where 
            (NGlobal arity code) = (hLookup (getHeap state) addr)

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

--showState will take the GCode and the stack from a given state and 
--wrap them in Iseqs
showState :: GMState -> Iseq
showState state
    = iConcat [showStack state, INewline
              ,showDump state, INewline
              ,showInstructions (getCode state), INewline]

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

shortShowInstructions :: Int -> GMCode -> Iseq
shortShowInstructions num code
    = iConcat [IStr "{", iInterleave (IStr "; ") dotcodes, IStr "}"]
    where
        codes = map showInstruction (take num code)
        dotcodes 
            | length code > num = codes ++ [IStr "..."]
            | otherwise         = codes

shortShowStack :: GMStack -> Iseq
shortShowStack stack
    = iConcat [IStr "["
              ,iInterleave (IStr ", ") (map (IStr . showAddr) stack)
              ,IStr "]"]

showNode :: GMState -> Addr -> Node -> Iseq
showNode state addr (NNum n)         = iNum n 
showNode state addr (NAp ad1 ad2)    = iConcat [IStr "Ap ", IStr (showAddr ad1)
                                               ,IStr " ", IStr (showAddr ad2)]
showNode state addr (NGlobal n code) = iConcat [IStr "Global ", IStr v]
                    where v = head [n | (n, b) <- getGlobals state, addr == b]
showNode state addr (NInd ad1)  = iConcat [IStr "Indirection ", IStr (showAddr ad1)]

showStats :: GMState -> Iseq
showStats state = iConcat [IStr "Steps taken: "
                          ,iNum (statGetSteps (getStats state))]
