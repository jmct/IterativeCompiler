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
                GMHeap,     --The Heap holding the program graph
                GMGlobals,  --Addresses for globals in the heap
                GMStats)    --Statistics about the computation

--The code for the GMCode type and `setter and getter' functions are below.
type GMCode = [Instruction]

data Instruction = 
          Unwind 
        | PushGlobal Name
        | PushInt Int
        | Push Int
        | MkAp
        | Slide Int
    deriving Eq

getCode :: GMState -> GMCode
getCode (code, _, _, _, _) = code

putCode :: CMCode -> GMState -> GMState
putCode code' (code, stack, heap, globals, stats) 
    = (code', stack, heap, globals, stats)


--The code for the GMStack is below:
type GMStack = [Addr]

getStack :: GMState -> GMStack
getStack (_, stack, _, _, _) = stack

putStack :: CMStack -> GMState -> GMState
putStack stack' (code, stack, heap, globals, stats) 
    = (code, stack', heap, globals, stats)

--The code for the state's heap:
type GMHeap = Heap Node

--A Node is either a number (a result), an application of two other nodes, or
--the arity & gCode sequence for when the global can be executed
data Node = 
          NNum Int
        | NAp Addr Addr
        | NGlobal Int GMCode


getHeap :: GMState -> GMHeap
getStack (_, _, heap, _, _) = heap

putHeap :: GMHeap -> GMState -> GMState
putHeap heap' (code, stack, heap, globals, stats) 
    = (code, stack, heap', globals, stats)


--The code for GMGlobals is below. Because the globals of a program do not
--change during execution, a `put' function is not needed
type GMGlobals = Assoc Name Addr

getGlobals :: GMState -> GMGlobals
getGlobals (_, _, _, globals, _) = globals

--GMStats:
type GMStats = Int

statInitial :: GMStats
statInitial = 0

statIncSteps :: GMStats -> GMStats
statIncSteps s = s + 1

statGetSteps :: GMStats -> Int
statGetSteps s = s

getStats :: GMState -> GMStats
getStats (code, stack, heap, globals, stats) = stats

putStats :: GMStats -> GMState -> GMState
putStats stats' (code, stack, heap, globals, stats) = 
        (code, stack, heap, globals, stats')

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
dispatch Unwind         = unwind

pushglobal :: Name -> GMState ->GMState
pushglobal f state 
    = putStack (a : getStack state) state
        where 
        a = aLookup (getGlobals state) f (error ("Undeclared global " ++ f))

pushint :: Int -> GMState -> GMState
pushint n state
    = putHeap heap' (putStack (a: getStack state) state)
    where (heap', a) =  hAlloc (getHeap state) (NNum n)

mkap :: GMState -> GMState
mkap state
    = putHeap heap' (putStack (a:as') state)
    where (heap', a) = hAlloc (getHeap state) (NAp a1 a2)
          (a1:a2:as') = getStack state

push :: Int -> GMState -> GMState
push n state
    = putStack (a:as) state
    where as = getStack state
          a  = getArg (hLookup (getHeap state) (as !! (n+1)))

getArg :: Node -> Addr
getArg (NAp a1 a2) = a2
getArg _ = error "Error in call to getArg: not an application node."

slide :: Int -> GMState -> GMState
slide n state
    = putStack (a: drop n as) state
    where (a:as) = getStack state

unwind :: GMState -> GMState
unwind state
    = newState (hLookup heap a)
    where
        (a:as) = getStack state
        heap   = getHeap state
        newState (NNum n) = state
        newState (NAp a1 a2) = putCode [Unwind] (putStack (a1:a:as) state)


