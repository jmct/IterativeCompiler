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

int main() {
    myHeap = malloc(HEAPSIZE * sizeof(HeapCell));
    myHeap[0].tag = APP;
    myHeap[0].app.leftArg = NULL;
    myHeap[0].app.rightArg = NULL;


    return 0;
}
