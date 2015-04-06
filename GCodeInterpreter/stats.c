#include <stdlib.h>
#include <string.h>
#include <gsl/gsl_statistics_uint.h>
#include "machine.h"
#include "stats.h"
#include "instruction_type.h"

void setupOpenLogFile(char *logFN, StatTable *table, int iter) {
    static int initialized = 0;

    if (table->quiet && !initialized) {
        initialized = 1;
        strcpy(logFN, table->lpName);
        table->lp = fopen(logFN, "w");
    } else if (!table->quiet) {
        char logCnt[15];
        sprintf(logCnt, ".%d", iter);
        strcpy(logFN, table->lpName);
        strcat(logFN, logCnt);
        table->lp = fopen(logFN, "w");
    }

    return;
}

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

void initTable(instruction * prog, unsigned int initSize, char *ltn, char *lpn, StatTable * table) {

    table->entries = malloc(sizeof(StatRecord) * initSize);
    table->ltName = ltn;
    table->lpName = lpn;
    table->progAddr = prog;
    table->size = initSize;
    table->currentEntry = 0;
    table->iteration += 1;

}


int logStats(StatTable * table) {
    char logFN[100];
    char logCnt[15];
    sprintf(logCnt, ".%d", table->iteration - 1);
    strcpy(logFN, table->ltName);
    strcat(logFN, logCnt);
    table->lt = fopen(logFN, "w");

    fprintf(table->lt, "#PSite\tTID\tLfspn\tReds\tBlckd\taTime\tCrtr\n");

    unsigned int n;
    for (n = 0; n < table->currentEntry; n++) {

        fprintf(table->lt,
            "%d\t%d\t%d\t%d\t%d\t%d\t%d\n",
            table->entries[n].parSite,
            table->entries[n].threadID, 
            table->entries[n].lifespan,
            table->entries[n].reductionCounter, 
            table->entries[n].blockedCounter,
            table->entries[n].aliveTime,
            table->entries[n].creatorID);
    }
    fclose(table->lt);
    return n;
}

void logStep(StatTable *table, unsigned long long int gReds, int nCores) {
    if (!table->quiet)
        fprintf(table->lp, "%llu\t%d\n", gReds, nCores);
}

void logIter(StatTable *table, unsigned long long int gReds, parSwitch *sws) {
    if (table->quiet) {
        fprintf(table->lp, "%d\t%llu\t", table->iteration - 1, gReds);
        int i;
        for (i = 0; sws[i].address > 0; i++) {
            fprintf(table->lp, "%d", sws[i].pswitch);
        }
        fprintf(table->lp, "\n");
    }
}

void endIterLog(StatTable *table) {
    if (!table->quiet)
        fclose(table->lp);
}

void closeLogFile(StatTable *table) {
    if (table->quiet)
        fclose(table->lp);
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

        statsArray[n].numThreads = right - left;

        left = right;
    }

    return statsArray;
}

void printParStats(ParSiteStats *st, int nps) {
    if (nps == 0)
        return;

    int n;
    for (n = 0; n <= nps; n++) {
        printf("For par site %u:\n", st[n].parSite);
        printf("\tNumber of threads sparked: %u\n", st[n].numThreads);
        printf("\tReduction Count mean: %f\n", st[n].rcMean);
        printf("\tBlocked Count mean: %f\n", st[n].bcMean);
        printf("\tLifespan mean: %f\n\n", st[n].lsMean);
    }

}
