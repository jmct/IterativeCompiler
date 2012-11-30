#include <stdio.h>
#include <stdlib.h>

#define HEAPSIZE 256

/* This typedef is just simple syntactic sugar for node tags */
typedef enum {
    FUN,
    APP,
    CONSTR,
    INTEGER, 
    COLLECTED,
    INDIRECTION
} Tag;

/* This typedef is just simple syntactic sugar for bools */
typedef enum {
    FALSE,
    TRUE
} Bool;

struct atom;
typedef struct atom HeapCell;

//This is once again syntactic sugar for declaring atoms.
//(Not having the say 'struct atom' instead just declare 'Atom')
typedef struct atom Atom;

/*The atom is the basic building block for items in the heap
 *
 *There are four 'types' of atoms: 
 *Integer, Application, Constructor, and Function
 *All atoms have a tag (based on the enumeration above).
 *The rest of the atom is made up of a union of the necessary
 *parts for the specific type. This means that even an atom tagged
 *as 'INTEGER' requires 3 words. One word for the tag, one word for the
 *int itself and one 'wasted' word that is there for use in the other atom
 *types. 
 */
struct atom {
    Tag tag;
    union {
        HeapCell * forward; //This is used for both Indirections and GC forwarding
        int num; 
        struct {
            HeapCell * leftArg;
            HeapCell * rightArg;
        } app;
        struct {
            int id;
            int arity;
        } constr; 
        struct {
            int arity;
            void * code;
        } fun;
    };
};


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

typedef HeapCell * Heap;
Heap myHeap;

int nextFree = 0;

Heap allocHeapCell(Tag tag, Heap heap) {
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

Heap allocApp(Heap left, Heap right) {
    Heap appNode = allocHeapCell(APP, myHeap);
    appNode->app.leftArg = left;
    appNode->app.rightArg = right;
    return appNode;
}

Heap allocConstr(int arity1, int id1) {
    Heap constrNode = allocHeapCell(CONSTR, myHeap);
    constrNode->constr.id = id1;
    constrNode->constr.arity = arity1;
    return constrNode;
}

Heap allocFun(int arity1, void* codePtr) {
    Heap funNode = allocHeapCell(FUN, myHeap);
    funNode->fun.arity = arity1;
    funNode->fun.code = codePtr;
    return funNode;
}

Heap allocInt(int value) {
    Heap intNode = allocHeapCell(INTEGER, myHeap);
    intNode->num = value;
    return intNode;
}

Heap allocIndirection(Heap forwardAdd) {
    Heap indNode = allocHeapCell(INDIRECTION, myHeap);
    indNode->forward = forwardAdd;
    return indNode;
}

int main() {
    myHeap = malloc(HEAPSIZE * sizeof(HeapCell));
    printf("Free: %d, Pointer Value %p\n", nextFree, myHeap);
    Heap point = allocHeapCell(APP, myHeap); 
    printf("Free: %d, Pointer Value %p\n", nextFree, point);
    point = allocHeapCell(APP, myHeap); 
    printf("Free: %d, Pointer Value %p\n", nextFree, point);

    return 0;
}
