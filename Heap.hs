module Heap where

type Addr = Int

--A heap is a collection of objects, each object has a corresponding address.
--The heap type will consist of the number of objects in the heap (the first
--Int), a `free-list' of unused addresses (the list of Ints), and lastly the
--list of objects (a tuple consisting of the address and the value).
type Heap a = (Int, [Addr], [(Addr, a)])

--To remove an item from the heap, we only really need to worry about how to 
--remove something from the contents (The association list). The other parts of
--the heap can be dealt with trivially.
remove :: [(Int, a)] -> Int -> [(Int, a)]
remove [] a = error ("Attempting to free or update a non-allocated address "
                     ++ showAddr a)
remove ((a',n) : contents) a
    | a == a' = contents
    | a /= a' = (a',n) : remove contents a

--The function creating the initial heap. (We know that there isn't actually
--infinite memory...)
hInitial :: Heap a
hInitial = (0, [1..] [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, (next:free), contents) n 
                                = ((size+1, free, (next,n) : contents), next)


hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, contents) a n = (size, free, (a,n) : remove contents a)


hFree :: Heap a -> Addr -> Heap a
hFree (size, free, contents) a = (size-1, a:free, remove contents a)

hLookup :: Heap a -> Addr -> a
hLookup (size, free, contents) addr
    = aLookup contents a (error ("Can't find node " ++ showAddr addr 
                                                    ++ " in heap"))

hAddresses :: Heap a -> [Addr]
hAddresses (_, _, contents) = [addr | (addr, node) <- contents]

hSize :: Heap a -> Int
hSize (size, _, _) = size

hNull :: Int
hNull = 0

hIsNull a = a == 0

showAddr :: Int -> String
showAddr a = "#" ++ show a
