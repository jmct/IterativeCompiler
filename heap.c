#include <stdio.h>
#include <stdlib.h>
#include "instructions.h"
#include "symbolTable.h"
#include "heap.h"


void showHeapItem(HeapCell item) {
    switch (item.tag) {
        case FUN:
            printf("FUN: arity %d, codePtr %d\n", 
                    item.fun.arity, item.fun.code);
            break;
        case LOCKED_FUN:
            printf("LOCKED FUN: arity %d, codePtr %d\n", 
                    item.fun.arity, item.fun.code);
            break;
        case APP:
            printf("APP: leftArf %p, rightArg %p\n", 
                    item.app.leftArg, item.app.rightArg);
            break;
        case LOCKED_APP:
            printf("LOCKED APP: leftArf %p, rightArg %p\n", 
                    item.app.leftArg, item.app.rightArg);
            break;
        case CONSTR:
            printf("Constr: arity %d, id %d\n", 
                    item.constr.arity, item.constr.id);
            break;
        case INTEGER:
            printf("INT: val %d\n", item.num);
            break;
        case INDIRECTION:
            printf("IND: Address %p\n", item.forward);
            break;
        default:
            printf("Indirection/forwarding MUST IMPLEMENT");
            break;
    } //end of switch statement
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

int nextFree = 0;

HeapPtr allocHeapCell(Tag tag, HeapPtr heap) {
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
            heap[nextFree].forward = NULL;
            break;
    } //end of switch statement
    return &heap[nextFree++];
}

HeapPtr allocApp(HeapPtr left, HeapPtr right, HeapPtr myHeap) {
    HeapPtr appNode = allocHeapCell(APP, myHeap);
    appNode->app.leftArg = left;
    appNode->app.rightArg = right;
    return appNode;
}

HeapPtr allocConstr(int arity1, int id1, HeapPtr myHeap) {
    HeapPtr constrNode = allocHeapCell(CONSTR, myHeap);
    constrNode->constr.id = id1;
    constrNode->constr.arity = arity1;
    return constrNode;
}

HeapPtr allocFun(int arity1, instruction * codePtr, HeapPtr myHeap) {
    HeapPtr funNode = allocHeapCell(FUN, myHeap);
    funNode->fun.arity = arity1;
    funNode->fun.code = codePtr;
    return funNode;
}

HeapPtr allocInt(int value, HeapPtr myHeap) {
    HeapPtr intNode = allocHeapCell(INTEGER, myHeap);
    intNode->num = value;
    return intNode;
}

HeapPtr allocIndirection(HeapPtr forwardAdd, HeapPtr myHeap) {
    HeapPtr indNode = allocHeapCell(INDIRECTION, myHeap);
    indNode->forward = forwardAdd;
    return indNode;
}

