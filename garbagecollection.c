#include "heap.h"
#include "stack.h"
#include "gthread.h"
#include "machine.h"

int garbageCollect(Heap* heap) {
    //declare the two `fingers'
    HeapPtr nextToInspect = NULL;
    heap->nextFreeCell = 0;

    //The current heap->toSpace, now needs to become the
    //new heap->fromSpace and visa versa
    //the nextToInspect can be used as a temp pointer for this since
    //it will point to the begining of the new toSpace initially anyway
    nextToInspect = heap->fromSpace;
    heap->fromSpace = heap->toSpace;
    heap->toSpace = heap->fromSpace;

    //traverse each stack of the active cores


    //traverse each stack of the machines in the thread pool
    
    
    //Now that all stacks have been traversed, inspect all pointers
    //that are stored in the heapCells that have been copied to the toSpace

}
