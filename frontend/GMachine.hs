module GMachine where
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
import Data.List

machineSize :: Int
machineSize = 4

runProg :: [Char] -> IO ()
runProg = putStr . showResult . eval . compile . parse

runShowProg :: [Char] -> IO ()
runShowProg = putStr . showResults . evals . compile . parse

runLogProg fileName = writeFile fileName . logStats . evals . compile . parse

maximum' [] = 0
maximum' xs  = maximum xs

--Similarly, for the parallel machine we have the following state tuple
type PGMState = (PGMGlobalState, --State shared by all threads
                 [PGMLocalState])--State for each thread

--The global state for the parallel machine is similar to the GMState for the
--sequential machine
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


--The Evaluator function takes a list of states (the first of which is created 
--by the compiler)
eval :: PGMState -> PGMState
eval state = nextState'
        where
        nextState' 
            | gmFinal state     = state
            | otherwise         = eval nextState
        nextState = doAdmin (steps state)

--The Evaluator function takes a list of states (the first of which is created 
--by the compiler)
evals :: PGMState -> [PGMState]
evals state = state : restStates
        where
        restStates 
            | gmFinal state     = []
            | otherwise         = evals nextState
        nextState = doAdmin (steps state)

--doAdmin allows for any between-state calculations that need to be made. In
--this case we increment the stats counter. 
doAdmin :: PGMState -> PGMState
doAdmin ((out, heap, globals, sparks, (taskInf, clocks)), local)
    = updateStats ((out, heap, globals, sparks, (taskInf, clocks')), local'')
      where (local'', clocks') = foldr filt ([], clocks) local'
            local' = filter isNotEmptyTask local
            filt (i, stack, dump, clock) (tasks, endClocks)
                | i == []   = (tasks, clock:endClocks)
                | otherwise = ((i, stack, dump, clock):tasks, endClocks)
            updateStats = nRunning . nIdle

isNotEmptyTask :: PGMLocalState -> Bool
isNotEmptyTask local =
    local /= emptyTask

--gmFinal state checks if there is any code left to execute; if there is not,
--we have reached the final state. 
gmFinal :: PGMState -> Bool
gmFinal s = snd s == [] && getPGMSparks s == []

--steps takes the entirety of the PGMState and carries out a 'step' on each
--thread by mapping the same step function from the sequential machine to each
--of the thread state's
--New tasks are only created if there is a free processor
steps :: PGMState -> PGMState
steps state
    = scheduler global' allTasks
      where ((out, heap, globals, sparks, stats), local) = state
            global'  = (out, heap, globals, [], stats)
            allTasks   = local ++ sparks
{-
steps :: PGMState -> PGMState
steps state
    = scheduler global' local'
      where ((out, heap, globals, sparks, stats), local) = state
            global'  = (out, heap, globals, sparks', stats)
            local'   = local ++ newtasks
            numTasks = length local
            sparks'   = drop (machineSize - numTasks) sparks
            newtasks = case numTasks < machineSize of
                            True -> take (machineSize - numTasks) 
                                         sparks
                            otherwise -> [] -}

--The scheduler function decides how to manage the currently open tasks amongst
--the machineSize (number of processors
scheduler :: PGMGlobalState -> [PGMLocalState] -> PGMState
scheduler global tasks
    = (global'', tasks')
      where running     = map tick (take machineSize tasks)
            nonRunning  = drop machineSize tasks
            (global', threads) = putPGMSparks nonRunning (global, tasks)
            (global'', tasks') = mapAccuml step global' running
{-
            tasks' = snd state'

            global'' = fst $ putPGMSparks (nonRunning ++ getPGMSparks state') state'-}

--Step ensures that the next instruction in a thread is executed by the
--appropriate function.
step :: PGMGlobalState -> PGMLocalState -> GMState
step global local = dispatch code (putCode rest state)
            where (code:rest) = getCode state
                  state       = (global, local)

--makeTask takes a pointer into the heap and creates a thread-state from that
--node
makeTask :: Addr -> PGMLocalState
makeTask addr = ([Eval], [addr], [], 0)

--tick increments the clock on each thread
tick :: PGMLocalState -> PGMLocalState
tick (i, stack, dump, clock) = (i, stack, dump, clock+1)

--Dispatch takes an instruction and converts it to a function call
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
dispatch Eval           = evalI
dispatch Add            = addI
dispatch Sub            = subI
dispatch Mul            = mulI
dispatch Div            = divI
dispatch Neg            = negI
dispatch Eq             = eqI
dispatch Ne             = neI
dispatch Lt             = ltI
dispatch Le             = leI
dispatch Gt             = gtI
dispatch Ge             = geI
dispatch (Cond al1 al2) = condI (al1, al2)
dispatch (Pack n1 n2)   = packI n1 n2
dispatch (Casejump alts)= casejump alts
dispatch (Split n)      = splitI n
dispatch Print          = printI
dispatch Par            = parI


--The Par instruction takes the current pointer from the top of the stack
--and adds it to the spark pool
parI :: GMState -> GMState
parI ((out, heap, globals, sparks, stats), (code, a:as, dump, clock))
    = ((out, heap, globals, (makeTask a):sparks, stats), (code, as, dump, clock))

printI :: GMState -> GMState
printI state
    = case (hLookup (getHeap state) a) of
        NNum n         -> putOutput (mkOutput n) (putStack (drop 1 $ getStack state) state)
        NConstr t ss -> putCode (mkCode ss) (putStack (mkStack ss) 
                                                        (putOutput (mkOutput' t) state))
        otherwise      -> error "Trying to print non-number or non-constructor"
      where mkOutput n  = (getOutput state) ++ show n ++ " "
            mkOutput' t = (getOutput state) ++ "<" ++ show t ++ ">" ++ " "
            mkCode ss   = (concat $ take (length ss) $ repeat [Eval, Print]) ++ (getCode state)
            (a:as)      = getStack state
            mkStack ss  = ss ++ as

getConstrTag :: Node -> Int
getConstrTag (NConstr tag ss) = tag
getConstrTag _                = error "Trying to get tag from non-constructor Node"

getConstrArgs :: Node -> [Addr]
getConstrArgs (NConstr tag ss) = ss
getConstrArgs _                = error "Trying to get arguments from non-constructor"

splitI :: Int -> GMState -> GMState
splitI n state 
    = if (length newAs == n) then putStack (newAs ++ as) state
                             else error "Arity of Constr does not match Split"
        where (a:as) = getStack state
              newAs  = getConstrArgs $ hLookup (getHeap state) a

casejump :: [(Int, GMCode)] -> GMState -> GMState
casejump alts state
    = putCode code' state
        where heap           = getHeap state
              (a:as)         = getStack state
              tag            = getConstrTag $ hLookup heap a
              selectCase a t = aLookup a t (error "Non-exhaustive patterns")
              code'          = (selectCase alts tag) ++ (getCode state)

packI :: Int -> Int -> GMState -> GMState
packI tag ar state 
    = putStack stack' (putHeap heap' state)
        where stack      = getStack state
              (heap', a) = hAlloc (getHeap state) (NConstr tag (take ar stack))
              stack'     = a:(drop ar stack)

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


--update changes the node of the result to an indirection node and ensures that
--all locked Applications are unlocked for other threads.
update :: Int -> GMState -> GMState
update n state
    = putStack (stack') (putHeap heap' state')
        where 
            (a:as)      = getStack state
            stack'      = drop 1 (a:as)
            an          = head (drop n stack')
            state'      = unlock an state
            heap'  = hUpdate (getHeap state') an (NInd a)

--The unlock function is used to signal that a reduction on a part of the graph
--is complete and that other threads can now use the node
unlock :: Addr -> GMState -> GMState
unlock addr state
    = newState (hLookup heap addr)
        where heap = getHeap state
              newState (NLAp a1 a2 blocked) 
                    = unlock a1 (putHeap (hUpdate heap addr (NAp a1 a2)) 
                                         (emptyPendingList blocked state))
              newState (NLGlobal n c blocked)
                    = putHeap (hUpdate heap addr (NGlobal n c)) 
                              (emptyPendingList blocked state)
              newState n = state

emptyPendingList :: [PGMLocalState] -> GMState -> GMState
emptyPendingList blocked state
    = nFreed (length blocked) $ putSparks (blocked ++ (getSparks state)) state

pop :: Int -> GMState -> GMState
pop n state = putStack stack' state
    where stack' = drop n (getStack state)

unwind :: GMState -> GMState
unwind state
    = newState (hLookup heap a)
    where
        (a:as) = getStack state
        heap   = getHeap state
        (dCode, dStack):dump' = getDump state
        k      = length as
        ak     = head $ drop k (a:as)
        (global, local) = state
        newState (NNum n)
                | null (getDump state) = state
                | otherwise            = putCode dCode (putStack (a:dStack) 
                                                        (putDump dump' state))
        newState (NConstr tag ss)
                | null (getDump state) = state
                | otherwise            = putCode dCode (putStack (a:dStack) 
                                                        (putDump dump' state))
        newState (NAp a1 a2) = putCode [Unwind] (putStack (a1:a:as) (lock a state))
        newState (NLAp a1 a2 blocked) = addToPending state       
        newState (NLGlobal n c blocked) = addToPending state
        newState (NInd a1) = putCode [Unwind] (putStack (a1:as) state)
        newState (NGlobal n c)
                | n == 0     = putCode c (lock a state)
                | k < n      = putCode dCode (putStack (ak:dStack) (putDump dump' state))
                | otherwise  = putStack (rearrange n (getHeap state) (a:as))
                                        (putCode c state)

addToPending :: GMState -> GMState
addToPending state@((out, heap, globals, sparks, stats), (code, a:as, dump, clock))
    = nBlocked 1 (global', emptyTask)
    where
        state' = putCode [Unwind] state
        (global, local) = state'
        heap'  = getHeap state'
        (global', local') = 
            case (hLookup heap a) of
                (NLAp a1 a2 blocked) -> putHeap (hUpdate heap' a 
                                                         (NLAp a1 a2 (local:blocked)))
                                                         state'
                (NLGlobal n c blocked) -> putHeap (hUpdate heap' a 
                                                         (NLGlobal n c (local:blocked)))
                                                         state'

emptyTask :: PGMLocalState
emptyTask = ([], [], [], 0)


--the lock function is used to lock a node that is being reduced by a thread
--so that other threads do not interfere for repeat the work
lock :: Addr -> GMState -> GMState
lock addr state
    = putHeap (newHeap (hLookup heap addr)) state
        where heap = getHeap state
              newHeap (NAp a1 a2) = hUpdate heap addr (NLAp a1 a2 [])
              newHeap (NGlobal n c)
                        | n == 0 = hUpdate heap addr (NLGlobal n c [])
                        | otherwise = heap


--evalI is the code to execute when evaluating the Eval instruction. This is not
--to be confused with the eval function, which is the evaluator (GMachine)
--itself
evalI :: GMState -> GMState
evalI state@((output, heap, globals, sparks, stats), (code, (a:as), dump, clock)) 
    = putCode [Unwind] (putStack [a] (putDump ((code, as):dump) state))

rearrange :: Int -> GMHeap -> GMStack -> GMStack
rearrange n heap stack
    = take n stack' ++ drop n stack
        where
            stack' = map (getArg . hLookup heap) (tail stack)

getArg :: Node -> Addr
getArg (NAp a1 a2) = a2
getArg (NLAp a1 a2 blocked) = a2
getArg _ = error "Error in call to getArg: not an application node."

--Below are the boxing and unboxing functions for items in the heap.
boxInteger :: Int -> GMState -> GMState
boxInteger num state
    = putStack (a : getStack state) (putHeap heap' state)
        where (heap', a) = hAlloc (getHeap state) (NNum num)

boxBoolean :: Bool -> GMState -> GMState
boxBoolean b state
    = putStack (a : getStack state) (putHeap heap' state)
        where
            (heap', a)     = hAlloc (getHeap state) (NConstr b' [])
            b' | b         = 1  --Tag for True
               | otherwise = 0  --Tag for False

unboxBoolean :: Addr -> GMState -> Bool
unboxBoolean addr state
    = unB (hLookup (getHeap state) addr)
        where unB (NConstr 1 _) = True
              unB (NConstr 0 _) = False
              unB _             = error "Trying to unboxBoolean on non-Boolean"

unboxInteger :: Addr -> GMState -> Int
unboxInteger ad state
    = unB (hLookup (getHeap state) ad)
        where unB (NNum num) = num
              unB _          = error "Unboxing a non integer"

--the primitive1 function takes a boxing function, an unboxing function and an
--operator (in this case unary) and a state to perform the operation on.
primitive1 :: (b -> GMState -> GMState) --the boxing function 
           -> (Addr -> GMState -> a)    --the unboxing
           -> (a -> b)                  --the operator
           -> GMState -> GMState        --input and output state
primitive1 box unbox op state
    = box (op (unbox a state)) (putStack as state)
        where (a:as) = getStack state

--the primitive2 function takes a boxing function, an unboxing function and an
--operator (in this case dyadic) and a state to perform the operation on.
primitive2 :: (b -> GMState -> GMState) --the boxing function 
           -> (Addr -> GMState -> a)    --the unboxing
           -> (a -> a -> b)             --the operator
           -> GMState -> GMState        --input and output state
primitive2 box unbox op state
    = box (op (unbox a1 state) (unbox a2 state)) (putStack as state)
        where (a1:a2:as) = getStack state

{-Now we use these primitives to create unboxing-reboxing functions specifically
 - for Integer arithmetic
 - -}
arithmetic1 :: (Int -> Int) -> GMState -> GMState
arithmetic1 = primitive1 boxInteger unboxInteger

arithmetic2 :: (Int -> Int -> Int) -> GMState -> GMState
arithmetic2 = primitive2 boxInteger unboxInteger

--For a comparison, we use the unboxInteger and the boxBoolean with the
--primitive2 function
comparison :: (Int -> Int -> Bool) -> GMState -> GMState
comparison = primitive2 boxBoolean unboxInteger

--Then each specific integer arithmetic function is just a wrapper for
--arithmatic{1,2} with the appropriate operator
addI :: GMState -> GMState
addI state
    = arithmetic2 (+) state

subI :: GMState -> GMState
subI state
    = arithmetic2 (-) state

mulI :: GMState -> GMState
mulI state
    = arithmetic2 (*) state

divI :: GMState -> GMState
divI state
    = arithmetic2 (div) state

negI :: GMState -> GMState
negI state
    = arithmetic1 (negate) state

eqI :: GMState -> GMState
eqI state
    = comparison (==) state

neI :: GMState -> GMState
neI state
    = comparison (/=) state

ltI :: GMState -> GMState
ltI state
    = comparison (<) state

leI :: GMState -> GMState
leI state
    = comparison (<=) state

gtI :: GMState -> GMState
gtI state
    = comparison (>) state

geI :: GMState -> GMState
geI state
    = comparison (<=) state

condI :: (GMCode, GMCode) -> GMState -> GMState
condI alts state
    = putCode (res ++ i) (putStack s state)
        where
            i   = getCode state
            a:s = getStack state
            b   = unboxBoolean a state 
            res | b == True    = fst alts
                | b == False   = snd alts
                | otherwise = error "Attempted boolean check on non-Boolean"

--mapAccuml is a utility function that will be used frequently in the code to
--come.
mapAccuml :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccuml f acc []      = (acc, [])
mapAccuml f acc (x:xs)  = (acc2, x':xs')
                where
                    (acc1, x')  = f acc x
                    (acc2, xs') = mapAccuml f acc1 xs


--Compile functions are broken into portions for compiling SC, R and C
compile :: CoreProgram -> PGMState
compile prog = (([], heap, globals, [], ((0,1,0),[])), [initialTask addr])
        where (heap, globals) = buildInitialHeap prog
              addr            = aLookupString globals "main" 
                                              (error "Main undefined")

--InitialTask takes an address and forms a LocalState structure
initialTask :: Addr -> PGMLocalState
initialTask addr = (initCode, [addr], [], 0)

--Once compiled a supercombinator for the G-Machine will be of the form:
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
      ,("par", 2, [Push 1, Push 1, Par, Update 2, Pop 2, Unwind])]
      ,("parOff", 2, [Push 1, Update 2, Pop 2, Unwind])]
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

--for above we need to map 'showInstruction over the list of each alternative
--and make it laid out nicelye

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
{- Below this is the code for logStats, which is used for benchmarking and
 - provides no tracing -}
logStats :: [PGMState] -> String
logStats states = concat $ intersperse "\n" 
                         $ map formatStats $ zip [1..] states

formatStats :: (Int, PGMState) -> String
formatStats (n, state) = concat $ intersperse "," [show n 
                                                  ,show blocked 
                                                  ,show running
                                                  ,show idle]
    where ((blocked, running, idle), count) = getPGMStats state
