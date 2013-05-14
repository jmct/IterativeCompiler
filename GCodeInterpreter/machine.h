#ifndef MACHINE_HEADER
#define MACHINE_HEADER

#include "stack.h"
#include "instructions.h"

struct Machine_ {
    stack stck;
    instruction* progCounter;
    //Below we have fields for the stats of each thread. They may not be used.
    //Instead we could print out to a log file at each important instance and 
    //use the log file to build up a 'picture' of each thread. 
    int reductionCounter, threadID, birthTime, deathTime;
};

typedef struct Machine_ Machine;
    

#endif
