#ifndef HEAP_H
#define HEAP_H
#include "instructions.h"
#include "gthread.h"
//#include "machine.h"


/* This typedef is just simple syntactic sugar for node tags */
typedef enum {
    FUN,
    LOCKED_FUN,
    APP,
    LOCKED_APP,
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
    HeapCell * gcForward; //for GC forwarding
    union {
        HeapCell * indirection; //for indirection Nodes
        int num; 
        struct {
            HeapCell * leftArg;
            HeapCell * rightArg;
            //List of suspended computations
            int numBlockedThreads;
            threadQueueNode* blockedQueue;
        } app;
        struct {
            int id;
            int arity;
            HeapCell ** fields;
        } constr; 
        struct {
            int arity;
            instruction * code;
            //List of suspended computations
            int numBlockedThreads;
            threadQueueNode* blockedQueue;
        } fun;
    };
};

typedef HeapCell * HeapPtr;

//This is a forward declaration of Machine_
//Actual definition is in machine.h
struct Machine_;

struct Heap_ {
    int nextFreeCell, maxSize, numCores;
    struct Machine_** activeCores;
    threadPool* thrdPool;
    HeapPtr toSpace;
    HeapPtr fromSpace;
};

typedef struct Heap_ Heap;

void addToBlockedQueue(struct Machine_* mach, HeapPtr heapItem);

void showHeapItem(HeapCell item);
HeapPtr allocHeapCell(Tag tag, Heap* globHeap);
HeapPtr allocApp(HeapPtr left, HeapPtr right, Heap* myHeap);
HeapPtr allocConstr(int id1, int arity1, Heap* myHeap);
HeapPtr allocFun(int arity1, instruction * codePtr, Heap* myHeap);
HeapPtr allocInt(int value, Heap* myHeap);
HeapPtr updateToInd(HeapPtr forwardAdd, HeapPtr node);
HeapPtr allocIndirection(HeapPtr forwardAdd, Heap* myHeap);

#endif
