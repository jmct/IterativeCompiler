#include <stdio.h>
#include "heap.h"
#include "stack.h"
#include "gthread.h"
#include "machine.h"
#include "garbagecollection.h"

int garbageCollect(Heap* heap, HeapPtr* additionalRoot1, HeapPtr* additionalRoot2) {
    //declare the two `fingers'
    HeapPtr nxtTI = NULL; //nxtTI stands for `next to inspect'
    heap->nextFreeCell = 0;

    //The current heap->toSpace, now needs to become the
    //new heap->fromSpace and visa versa
    //the nxtTI can be used as a temp pointer for this since
    //it will point to the begining of the new toSpace initially anyway
    nxtTI = heap->fromSpace;
    heap->fromSpace = heap->toSpace;
    heap->toSpace = nxtTI;

    int i;

    //traverse each stack of the active cores
    for (i = 0; i < heap->numCores; i++) {
        if (heap->activeCores[i] != NULL )
            collectMachine(heap->activeCores[i], heap);
    }

    //traverse the thread pool
    threadQueueNode* curNode = heap->thrdPool->head;
    for (i = 0; i < heap->thrdPool->numThreads; i++) {
        if (curNode->current == NULL) {
            printf("In GC: There was a node that pointed to a NULL machine in the thread pool\n");
        }
        collectMachine(curNode->current, heap);
        curNode = curNode->next;
    }

    if (additionalRoot1 != NULL) {
        *additionalRoot1 = copyHeapItem(*additionalRoot1, globalHeap);
    }
    if (additionalRoot2 != NULL) {
        *additionalRoot2 = copyHeapItem(*additionalRoot2, globalHeap);
    }
    //Now that all stacks have been traversed, inspect all pointers
    //that are stored in the heapCells that have been copied to the toSpace
    
    i = 0; //now used as `finished' flag
    while(i == 0) {
        switch (nxtTI->tag) {
            int j; //used for looping through CONSTR fields and blocked_queues
            case APP:
            case LOCKED_APP:
                nxtTI->app.leftArg = copyHeapItem(nxtTI->app.leftArg, heap);
                nxtTI->app.rightArg = copyHeapItem(nxtTI->app.rightArg, heap);
                curNode = nxtTI->app.blockedQueue;
                for (j = 0; j < nxtTI->app.numBlockedThreads; j++) {
                    if (curNode->current == NULL) {
                        printf("In GC: A node pointed to a NULL machine in app's blocked queue\n");
                    }
                    collectMachine(curNode->current, heap);
                    curNode = curNode->next;
                }
                break;
            case CONSTR:
                for (j = 0; j < nxtTI->constr.arity; j++) {
                    nxtTI->constr.fields[j] = copyHeapItem(nxtTI->constr.fields[j], heap);
                }
                break;
            case INDIRECTION:
                nxtTI->indirection = copyHeapItem(nxtTI->indirection, heap);
                break;
            case FUN:
            case LOCKED_FUN:
                curNode = nxtTI->fun.blockedQueue;
                for (j = 0; j < nxtTI->fun.numBlockedThreads; j++) {
                    if (curNode->current == NULL) {
                        printf("In GC: A node pointed to a NULL machine in fun's blocked queue\n");
                    }
                    collectMachine(curNode->current, heap);
                    curNode = curNode->next;
                }
                break;
            case COLLECTED:
                printf("I don't care if the program `runs' correctly, seeing this means there's a bug in GC");
                break;
            case INTEGER:
                break;
            default:
                printf("Reached default case in GC switch, this shouldn't happen\n");
        }
        nxtTI += 1;
        if (nxtTI == &heap->toSpace[heap->nextFreeCell]) {
            i = 1;
        }
    }
    return heap->nextFreeCell;
}

int numCopied;
void collectMachine(Machine* curMach, Heap* heap) {
    int finished;
    numCopied = 0;
    HeapPtr * fakeFramePtr, * fakeStackPtr;
    chunk* currentChunk = curMach->stck.stackObj;
    fakeFramePtr = curMach->stck.framePointer;
    fakeStackPtr = curMach->stck.stackPointer;
    finished = 0;
    while (finished == 0) {
        *fakeStackPtr = copyHeapItem(*fakeStackPtr, heap);
        fakeStackPtr = getNthAddrFrom(1, &curMach->stck, fakeStackPtr);
        if (fakeStackPtr == fakeFramePtr) {
            //get new framepointer value
            fakeFramePtr = (HeapPtr*)(*fakeFramePtr);
            //skip the activation record
            fakeStackPtr = getNthAddrFrom(3, &curMach->stck, fakeStackPtr);
        }
        finished = isPtrAtEndOfStack(&curMach->stck, fakeStackPtr);
    }
}

HeapPtr copyHeapItem(HeapPtr item, Heap* heap) {
    if (item->tag != COLLECTED) {
        heap->toSpace[heap->nextFreeCell] = *item;
        item->tag = COLLECTED;
        item->gcForward = &(heap->toSpace[heap->nextFreeCell]);
        heap->nextFreeCell += 1;
        numCopied += 1;
    }
    return item->gcForward;
}

