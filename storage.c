#include <stdio.h>

/* This typedef is just simple syntactic sugar for node tags */
typedef enum {
    FUN,
    APP,
    CONSTR,
    INTEGER 
} Tag;

/* This typedef is just simple syntactic sugar for bools */
typedef enum {
    FALSE,
    TRUE
} Bool;

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
        int num; 
        struct atom * app;
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

//This is once again syntactic sugar for declaring atoms.
//(Not having the say 'struct atom' instead just declare 'Atom')
typedef struct atom Atom;

/*Because there is a bit of dependency recursion in the definiton of
 *a heapcell we have to provide a prototype definition. Basically, a heapcell
 *is either a header of an atom. All heapCells start off with a header, and
 *then have a variable number of atoms succeeding the header. A binary application
 *will most likely take the following form:
 *
 *
 * +--------+
 * | HC ptr |
 * +---+----+
 *     |        0      1     2
 *     |      +-----+-----+-----+
 *     |      |FALSE|App: |App: |
 *     +----->|_____|_____|_____|
 *            |Size:|Addr:|Addr:|               Y   (Y+1)
 *            |  2  |  X  |  Y  |            +-----+-----+
 *            +-----+--+--+--+--+            |FALSE|Int: |
 *                     |     |               |_____|     |
 *                     |     +-------------->|Size:|  5  |
 *                     |                     |  1  |     |
 *                     |      X     (X+1)    +-----+-----+
 *                     +-->+-----+--------+
 *                         |FALSE|Fun:    |
 *                         |_____|________|
 *                         |Size:|Arity: 1|
 *                         |  1  |Code ptr|
 *                         +-----+--------+
 * */
union heapcell;
typedef union heapcell HeapCell;

/*A Header is used for the GC. 
 *The 'collected' Bool tells the GC whether this HeapCell has been collected
 *yet. If it has, then the HeapCell pointer is used from the union. This will
 *point to the new heap address of the HeapCell. If it has not been collected
 *then the Size of the HeapCell is used from the Union. This specifies how many
 *Atoms make up the HeapCell
 */
typedef struct {
  Bool collected;
  union {
       HeapCell *forward;
       int size;
  };
} Header;

/*This is the actual definition of a heapcell. As you can see, the header */
union heapcell {
    Header header;
    Atom atom;
};


typedef HeapCell * Heap;

HeapCell myHeap[16];

int main() {
    myHeap[0].header.collected = FALSE;
    myHeap[0].header.size = 2;
    myHeap[1].atom.tag = APP;
    myHeap[1].atom.fun.arity = 1;
    myHeap[1].atom.fun.code = NULL;
    myHeap[2].atom.tag = INTEGER;
    myHeap[2].atom.num = 2;


    return 0;
}
