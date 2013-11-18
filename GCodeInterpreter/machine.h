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
    //
    //reductionCounter, birthTime, deathTime and threadID are self described.
    unsigned int reductionCounter, threadID, birthTime, creatorID; 
    /* The parSite field is for recording which par annotation is resonsible for
     * creating this thread. Unfortunately we also need the temporary field so
     * that the G-Machine can correctly annotate nodes with the parSite
     * responsible for the thread's children
     */
    instruction * parSite, *childrensParSite;

    //aliveCounter and blockCounter work together to give us the the total
    //number of reductions 'worked' for this thread. Whenever the thread blocks
    //we subtract the global reduction counter from the unblockTime, giving us
    //the number of reductions this thread has been alive for and we add that to
    //the aliveCounter. When the thread is freed we update the unblockTime to
    //reflect that.
    unsigned int unblockTime, aliveCounter, blockedCounter;
};

typedef struct Machine_ Machine;
    

#endif
