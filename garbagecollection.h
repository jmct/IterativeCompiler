#ifndef GARBAGE_COLLECTION
#define GARBAGE_COLLECTION
#include "heap.h"
#include "stack.h"
#include "gthread.h"
#include "machine.h"


//Returns the number of heapCells transfered over to the new heap space
//reurns -1 if there was an error
int garbageCollect(Heap* heap, HeapPtr* additionalRoot1, HeapPtr* additionalRoot2);

void collectMachine(Machine* curMach, Heap* heap);

HeapPtr copyHeapItem(HeapPtr item, Heap* heap);






#endif
