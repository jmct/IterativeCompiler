module Heap where

type Addr = Int

--A heap is a collection of objects, each object has a corresponding address.
--The heap type will consist of the number of objects in the heap (the first
--Int), a `free-list' of unused addresses (the list of Ints), and lastly the
--list of objects (a tuple consisting of the address and the value).
type Heap a = (Int, [Addr], [(Addr, a)])

--The function creating the initial heap. (We know that there isn't actually
--infinite memory...)
hInitial :: Heap a
hInitial = (0, [1..] [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, (next:free), contents) n = ((size+1, free, (next,n) : contents), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hFree :: Heap a -> Addr -> Heap a
