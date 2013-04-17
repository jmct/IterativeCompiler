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
    int i;
    for (i = 0; i < heap->numCores; i++) {

        //TODO implement a new funcion itemsInFakeFrame so that we can loop
        //around in each frame and collect all the references


    //traverse each stack of the machines in the thread pool
    
    
    //Now that all stacks have been traversed, inspect all pointers
    //that are stored in the heapCells that have been copied to the toSpace

}

HeapPtr copyHeapItem(HeapPtr item, Heap* heap) {
    if (item->tag != COLLECTED) {
        heap->toSpace[heap->nextFreeCell] = *item;
        item->tag = COLLECTED;
        item->gcForward = &(heap->toSpace[heap->nextFreeCell]);
        heap->nextFreeCell += 1;
    }
    return item->gcForward;
}

