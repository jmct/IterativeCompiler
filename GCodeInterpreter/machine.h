#ifndef MACHINE_HEADER
#define MACHINE_HEADER

#include "stack.h"
#include "heap.h"
#include "instruction_type.h"

enum _ThreadStatus {
    NEW,
    WAS_BLOCKED,
    RUNNING
};

typedef enum _ThreadStatus ThreadStatus;

struct Machine_ {
    stack stck;
    ThreadStatus status;
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

    /* The delays intended to simulate the overhead of scheduling and managing 
     * threads. 
     *
     * initOH -> The overhead that a *child* thread has before it can start com
     *           computing
     *
     * creationOH -> The overhead that a *parent* thread has before it creates
     *               a child thread
     */
    unsigned int initOH;
    unsigned int creationOH;
};

typedef struct Machine_ Machine;
    
void initMachine(Machine *mach, unsigned int iOH, unsigned int cOH);

unsigned int executeProg(parSwitch* s, instruction* p, int c);

int isCleanStack(Machine *mach, Heap* heap);

#endif
