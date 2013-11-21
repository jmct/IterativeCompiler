#ifndef MACHINE_HEADER
#define MACHINE_HEADER

#include "stack.h"
#include "instructions.h"

struct Machine_ {
    stack stck;
    instruction* progCounter;
    
    /*TODO Profiling Only */
    //Below we have fields for the stats of each thread. They may not be used.
    //Instead we could print out to a log file at each important instance and 
    //use the log file to build up a 'picture' of each thread. 
    //
    //reductionCounter, birthTime, deathTime and threadID are self described.
    unsigned int reductionCounter, threadID, birthTime, creatorID; 
    /* The parSite field is for recording which par annotation is resonsible for
     * creating this thread. Unfortunately we also need the temporary field so
     * that the G-Machine can correctly annotate nodes with the parSite
     * responsible for the thread's children
     */
    instruction * parSite, *childrensParSite;

    /* When a thread is blocked, we set the blockTime to the globalReductions
     * counter, then when it unblocks we subtrack the GRC from the blocktime and
     * add that result to blockedCounter
     */
    unsigned int blockTime, blockedCounter;
};

typedef struct Machine_ Machine;
    

#endif
