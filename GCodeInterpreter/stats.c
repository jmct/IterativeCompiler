#include <stdlib.h>
#include "machine.h"
#include "stats.h"
#include "instructions.h"

int recordMach(Machine* mach, StatTable* table, 
               unsigned int lifespan) {

   int aliveTime = lifespan - mach->blockedCounter;


    /* Enter stats into table */
    /*TODO we need function that enters a mach's stats into the table */

   /* check that the new entry will fit in the table */
   if (table->currentEntry == (table->size - 1)) {
       table->entries = realloc(table->entries, sizeof(StatRecord) * table->size * 2);
       table->size *= 2;
   }

   /* temp variable to make assignments easier to read and understand */
   int curEntry = table->currentEntry;
   StatRecord * newRecord = &(table->entries[curEntry]);

   /* write data to the record */
   newRecord->parSite = mach->parSite - table->progAddr;
   newRecord->threadID = mach->threadID;
   newRecord->reductionCounter = mach->reductionCounter;
   newRecord->lifespan = lifespan;
   newRecord->blockedCounter = mach->blockedCounter;
   newRecord->aliveTime = aliveTime;
   newRecord->creatorID = mach->creatorID;

   table->currentEntry += 1;

    return 0;
    
}

void initTable(instruction * prog, unsigned int initSize, StatTable * initTable) {

    initTable->entries = malloc(sizeof(StatRecord) * initSize);
    initTable->progAddr = prog;
    initTable->size = initSize;
    initTable->currentEntry = 0;

}


int logStats(StatTable * table, FILE * logFile) {

    fprintf(logFile, "ParSite,ThreadID,Lifespan,Reductions,BlockedCntr,aliveTime,Creator\n");

    unsigned int n;
    for (n = 0; n < table->currentEntry; n++) {

        fprintf(logFile,
            "%d,%d,%d,%d,%d,%d,%d\n",
            table->entries[n].parSite,
            table->entries[n].threadID, 
            table->entries[n].lifespan,
            table->entries[n].reductionCounter, 
            table->entries[n].blockedCounter,
            table->entries[n].aliveTime,
            table->entries[n].creatorID);
    }
    return n;
}
    

int compare_entries(const void *e1, const void *e2) {

    const unsigned int parSite1 = ((const StatRecord *)e1)->parSite;
    const unsigned int parSite2 = ((const StatRecord *)e2)->parSite;

    return (parSite1 > parSite2) - (parSite1 < parSite2);
}
