#include "machine.h"
#include "stats.h"

int recordMach(Machine* mach, StatTable* table, 
               FILE* logFile, unsigned int lifespan) {

   int aliveTime = lifespan - mach->blockedCounter;

    /* Print stats to CSV file */
   fprintf(logFile,
           "%d,%d,%d,%d,%d,%d\n",
           mach->threadID, 
           lifespan,
           mach->reductionCounter, 
           mach->blockedCounter,
           aliveTime,
           mach->creatorID);

    /* Enter stats into table */
    /*TODO we need function that enters a mach's stats into the table */
    return 0;
    
}
