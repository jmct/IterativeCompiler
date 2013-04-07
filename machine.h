#ifndef MACHINE_HEADER
#define MACHINE_HEADER

#include "stack.h"
#include "instructions.h"

struct Machine_ {
    stack stck;
    instruction* progCounter;
};

typedef struct Machine_ Machine;
    

#endif
