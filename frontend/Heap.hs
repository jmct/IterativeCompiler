module Heap where

import Data.IntMap as M

--We need to define an association list type for use in our heap
--An association will be a tuple _associating_ the value of one type to the a
--stored value of another type. 
type Assoc a b = [(a,b)]

aEmpty = []

--When lookup up a value in an association list, we want a default value that
--will be the result if the key is not found in the list. Usually this will be
--an error message, but it could be anyting of type b.
aLookup :: Assoc Int b -> Int -> b -> b
aLookup [] key defalt = defalt
aLookup ((key, val):rest) key' defalt
    | key == key' = val
    | key /= key' = aLookup rest key' defalt

--This aLookup was needed because the previous aLookup was not allowing abstract
--datatypes for the key values (because they wouldn't necessarilly have
--instances of Eq)
aLookupString :: Assoc String b -> String -> b -> b
aLookupString [] key defalt = defalt
aLookupString ((key, val):rest) key' defalt
    | key == key' = val
    | key /= key' = aLookupString rest key' defalt
--Not directly used in our heap implementation, but useful to have when dealing
--with association lists, we've created functions to find the domain (the list
--of valid key values) and the range (the list of stored values in the
--association list) of an Assoc list. 
aDomain :: Assoc a b -> [a]
aDomain alist = [key | (key,val) <- alist]

aRange :: Assoc a b -> [b]
aRange alist = [val | (key,val) <- alist]


type Addr = Int

--A heap is a collection of objects, each object has a corresponding address.
--The heap type will consist of the number of objects in the heap (the first
--Int), a `free-list' of unused addresses (the list of Ints), and lastly the
--list of objects (a tuple consisting of the address and the value).
type Heap a = ([Addr], M.IntMap a)

--To remove an item from the heap, we only really need to worry about how to 
--remove something from the contents (The association list). The other parts of
--the heap can be dealt with trivially.
remove :: M.IntMap a -> Int -> M.IntMap a
remove = flip M.delete

--The function creating the initial heap. (We know that there isn't actually
--infinite memory...)
hInitial :: Heap a
hInitial = ([1..], M.empty)

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc ((next:free), contents) n = ((free, M.insert next n contents), next)


hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (free, contents) addr v = (free, M.insert addr v contents)


hFree :: Heap a -> Addr -> Heap a
hFree (free, contents) a = (a:free, remove contents a)

hLookup :: Heap a -> Addr -> a
hLookup (free, contents) addr
    = let res = M.lookup addr contents
      in case res of
        Just a  -> a 
        Nothing -> (error ("Can't find node " ++ showAddr addr  ++ " in heap"))

hAddresses :: Heap a -> [Addr]
hAddresses (_, contents) = M.keys contents

hSize :: Heap a -> Int
hSize (_, conts) = M.size conts

hNull :: Int
hNull = 0

hIsNull a = a == 0

showAddr :: Int -> String
showAddr a = "#" ++ show a
