#include <stdlib.h>
#include <gsl/gsl_statistics_uint.h>
#include "machine.h"
#include "stats.h"
#include "instruction_type.h"

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

void initTable(instruction * prog, unsigned int initSize, FILE *lf, StatTable * initTable) {

    initTable->entries = malloc(sizeof(StatRecord) * initSize);
    initTable->lf = lf;
    initTable->progAddr = prog;
    initTable->size = initSize;
    initTable->currentEntry = 0;

}


int logStats(StatTable * table) {

    fprintf(table->lf, "ParSite,ThreadID,Lifespan,Reductions,BlockedCntr,aliveTime,Creator\n");

    unsigned int n;
    for (n = 0; n < table->currentEntry; n++) {

        fprintf(table->lf,
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

/* Our goal is to create an array where each element represents the statistics
 * of a specific parSite. Passing in the number of parSites makes it easier
 * since we can just allocate the correct size right at the begining.
 */
ParSiteStats * calcParSiteStats(StatTable* statTable, int numParSites) {
    if (statTable->currentEntry == 0 || numParSites == 0)
        return NULL;

    ParSiteStats * statsArray = malloc(sizeof(ParSiteStats) * numParSites);

    StatRecord * stats = statTable->entries;
    StatRecord * maxEntry = statTable->entries + statTable->currentEntry;
    unsigned int curParSite;

    /* Set up two pointers that will form the boundary of the sub-arrays for
     * each ParSite.
     */
    StatRecord * left = stats, * right = stats;

    /*loop that measures the bounds of each sub-array and sends it off to get
     * the stats measured
     */
    int n;
    for(n = 0; n < numParSites; n++) {

        curParSite = left->parSite;

        while (right < maxEntry && right->parSite == curParSite) {
            right += 1;
        }

        statsArray[n].parSite = curParSite;
        statsArray[n].rcMean = gsl_stats_uint_mean(&left->reductionCounter,
                                                   sizeof(StatRecord)/sizeof(int),
                                                   (size_t)(right - left));
        statsArray[n].bcMean = gsl_stats_uint_mean(&left->blockedCounter,
                                                   sizeof(StatRecord)/sizeof(int),
                                                   (size_t)(right - left));
        statsArray[n].lsMean = gsl_stats_uint_mean(&left->lifespan,
                                                   sizeof(StatRecord)/sizeof(int),
                                                   (size_t)(right - left));

        left = right;
    }

    return statsArray;
}

void printParStats(ParSiteStats *st, int nps) {
    if (nps == 0)
        return;

    int n;
    for (n = 0; n <= nps; n++) {
        printf("For par site %d:\n", n);
        printf("\tReduction Count mean: %f\n", st[n].rcMean);
        printf("\tBlocked Count mean: %f\n", st[n].bcMean);
        printf("\tLifespan mean: %f\n\n", st[n].lsMean);
    }

}
