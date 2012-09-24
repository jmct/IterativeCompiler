#include <stdio.h>

typedef enum {
    FUN,
    APP,
    CONSTR,
    INTEGER 
} Tag;

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

typedef struct atom Atom;

union heapcell;

typedef union heapcell HeapCell;

typedef struct {
  int collected;
  union {
       HeapCell *forward;
       int size;
  };
} Header;

union heapcell {
    Header header;
    Atom atom;
};


typedef HeapCell * Heap;

HeapCell myHeap[16];

int main() {
    myHeap[0].header.collected = 0;
    myHeap[0].header.size = 2;
    myHeap[1].atom.tag = APP;
    myHeap[1].atom.fun.arity = 1;
    myHeap[1].atom.fun.code = NULL;
    myHeap[2].atom.tag = INTEGER;
    myHeap[2].atom.num = 2;


    return 0;
}
