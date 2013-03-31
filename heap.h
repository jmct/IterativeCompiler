#ifndef HEAP_H
#define HEAP_H
#include "instructions.h"


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
    union {
        HeapCell * forward; //This is used for both Indirections and GC forwarding
        int num; 
        struct {
            HeapCell * leftArg;
            HeapCell * rightArg;
            //TODO LIst of suspended computations
        } app;
        struct {
            int id;
            int arity;
        } constr; 
        struct {
            int arity;
            instruction * code;
            //TODO list of suspended computations
        } fun;
    };
};

typedef HeapCell * HeapPtr;

void showHeapItem(HeapCell item);
HeapPtr allocHeapCell(Tag tag, HeapPtr heap);
HeapPtr allocApp(HeapPtr left, HeapPtr right, HeapPtr myHeap);
HeapPtr allocConstr(int arity1, int id1, HeapPtr myHeap);
HeapPtr allocFun(int arity1, instruction * codePtr, HeapPtr myHeap);
HeapPtr allocInt(int value, HeapPtr myHeap);
HeapPtr allocIndirection(HeapPtr forwardAdd, HeapPtr myHeap);

#endif
