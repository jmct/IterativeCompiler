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









