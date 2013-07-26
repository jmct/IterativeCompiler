#include <stdio.h>
#include <stdlib.h>
#include "instructions.h"
#include "symbolTable.h"
#include "heap.h"
#include "gthread.h"
#include "garbagecollection.h"

int isAddrInToSpace(HeapPtr addr, Heap* heap) {
    if (addr >= heap->toSpace && addr <= &heap->toSpace[heap->maxSize - 1])
        return 1;
    else
        return 0;
}

void showHeapItem(HeapCell *item) {
    switch (item->tag) {
        case FUN:
            printf("FUN @%p: arity %d, codePtr %p\n", item, 
                    item->fun.arity, item->fun.code);
            break;
        case LOCKED_FUN:
            printf("LOCKED FUN @%p: arity %d, codePtr %p\n", item, 
                    item->fun.arity, item->fun.code);
            break;
        case APP:
            printf("APP @%p: leftArf %p, rightArg %p\n", item, 
                    item->app.leftArg, item->app.rightArg);
            break;
        case LOCKED_APP:
            printf("LOCKED APP @%p: leftArf %p, rightArg %p\n", item, 
                    item->app.leftArg, item->app.rightArg);
            break;
        case CONSTR:
            printf("Constr @%p: arity %d, id %d\n", item, 
                    item->constr.arity, item->constr.id);
            break;
        case INTEGER:
            printf("INT @%p: val %d\n", item, item->num);
            break;
        case INDIRECTION:
            printf("IND @%p: Address %p\n", item, item->indirection);
            break;
        default:
            printf("Indirection/forwarding MUST IMPLEMENT");
            break;
    } //end of switch statement
}

void showHeap(Heap* heap) {
    printf("ITEMS IN HEAP: \n\n");
    int i;
    for (i = 0; i < heap->nextFreeCell; i++) {
        showHeapItem(&(heap->toSpace[i]));
    }
}

/*Because there is a bit of dependency recursion in the definiton of
 *a heapcell we have to provide a prototype definition. Basically, a heapcell
 *is either a header of an atom. All heapCells start off with a header, and
 *then have a variable number of atoms succeeding the header. A binary application
 *will most likely take the following form:
 *
 *
 *    Binary Applications:
 *
 *       +----------+
 *       |HC Pointer|
 *       +-----+----+             +--------+-------+
 *             |                  |FUN     |INT    |
 *             |              +-->|Arity: 1|Val: 5 |
 *             |  +--------+  |   |CodePtr |       |
 *             |  |APP     |  |   +--------+-------+
 *             +->|leftArg +--+               ^
 *                |        |                  |
 *                |rightArg+------------------+
 *                +--------+
 * */

/*A Header is used for the GC. 
 *The 'collected' Bool tells the GC whether this HeapCell has been collected
 *yet. If it has, then the HeapCell pointer is used from the union. This will
 *point to the new heap address of the HeapCell. If it has not been collected
 *then the Size of the HeapCell is used from the Union. This specifies how many
 *Atoms make up the HeapCell
 */


HeapPtr allocHeapCell(Tag tag, Heap* globHeap, HeapPtr* first, HeapPtr* second) {
    int nextFree = globHeap->nextFreeCell;
    if (nextFree >= globHeap->maxSize) {
      //  printf("Trying GC!\n");
        nextFree = garbageCollect(globHeap, first, second);
      //  printf("%d Items copied during GC\n", nextFree);
    }
    HeapPtr heap = globHeap->toSpace;
    heap[nextFree].tag = tag;
    switch (tag) {
        case FUN:
            heap[nextFree].fun.arity = -1;
            heap[nextFree].fun.code  = NULL;
            break;
        case APP:
            heap[nextFree].app.leftArg = NULL;
            heap[nextFree].app.rightArg = NULL;
            break;
        case CONSTR:
            heap[nextFree].constr.id = -1;
            heap[nextFree].constr.arity = -1;
            break;
        case INTEGER:
            heap[nextFree].num = 0;
            break;
        default:
            heap[nextFree].indirection = NULL;
            break;
    } //end of switch statement
    globHeap->nextFreeCell += 1;
    return &heap[nextFree];
}

HeapPtr allocApp(HeapPtr left, HeapPtr right, Heap* myHeap) {
    HeapPtr appNode = allocHeapCell(APP, myHeap, &left, &right);
    appNode->app.leftArg = left;
    appNode->app.rightArg = right;
    appNode->app.numBlockedThreads = 0;
    appNode->app.blockedQueue = NULL;
    return appNode;
}

HeapPtr allocConstr(int id1, int arity1, Heap* myHeap) {
    HeapPtr constrNode = allocHeapCell(CONSTR, myHeap, NULL, NULL);
    constrNode->constr.id = id1;
    constrNode->constr.arity = arity1;
    if (arity1 > 0)
        constrNode->constr.fields = malloc(sizeof(HeapCell*) * arity1);
    else 
        constrNode->constr.fields = NULL;
    return constrNode;
}

HeapPtr allocFun(int arity1, instruction * codePtr, Heap* myHeap) {
    HeapPtr funNode = allocHeapCell(FUN, myHeap, NULL, NULL);
    funNode->fun.arity = arity1;
    funNode->fun.code = codePtr;
    funNode->fun.numBlockedThreads = 0;
    funNode->fun.blockedQueue = NULL;
    return funNode;
}

HeapPtr allocInt(int value, Heap* myHeap) {
    HeapPtr intNode = allocHeapCell(INTEGER, myHeap, NULL, NULL);
    intNode->num = value;
    return intNode;
}

HeapPtr updateToInd(HeapPtr forwardAdd, HeapPtr node) {
    node->tag = INDIRECTION;
    node->indirection = forwardAdd;
    return node;
}

HeapPtr allocIndirection(HeapPtr forwardAdd, Heap* myHeap) {
    HeapPtr indNode = allocHeapCell(INDIRECTION, myHeap, &forwardAdd, NULL);
    indNode->indirection = forwardAdd;
    return indNode;
}

void addToBlockedQueue(struct Machine_* mach, HeapPtr heapItem) {
    threadQueueNode* newNode = malloc(sizeof(threadQueueNode));
    newNode->current = mach;
    if (heapItem->tag == LOCKED_APP) {
        newNode->next = heapItem->app.blockedQueue;
        heapItem->app.blockedQueue = newNode;
        heapItem->app.numBlockedThreads += 1;
    }
    else if (heapItem->tag == LOCKED_FUN) {
        newNode->next = heapItem->fun.blockedQueue;
        heapItem->fun.blockedQueue = newNode;
        heapItem->fun.numBlockedThreads += 1;
    }
}
