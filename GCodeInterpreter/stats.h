#ifndef STATS_HEADER
#define STATS_HEADER
#include "machine.h"
#include "instruction_type.h"


/* The structures below are only used when profiling
 * TODO make these definitions depend on a 'profiling' flag
 */
struct _StatRecord {
    unsigned int parSite, threadID, reductionCounter, lifespan;
    unsigned int blockedCounter, aliveTime, creatorID;
};

typedef struct _StatRecord StatRecord;

struct _StatTable {
    unsigned int size, currentEntry;
    FILE *lf;
    instruction * progAddr;
    StatRecord * entries;
};

typedef struct _StatTable StatTable;

struct _ParSiteStats {
    unsigned int parSite, numThreads;
    double rcMean, bcMean, lsMean;
};

typedef struct _ParSiteStats ParSiteStats;

/*Profiling stat funtions */
int recordMach(Machine* mach, StatTable* table, 
               unsigned int lifepsan);

void initTable(instruction* prog, unsigned int initSize, FILE *lf, StatTable * table);

int logStats(StatTable * table);

int compare_entries(const void *e1, const void *e2);

ParSiteStats * calcParSiteStats(StatTable* statTable, int numParSites);

void printParStats(ParSiteStats *st, int nps);

/* Global Variables! */
extern unsigned int globalReductions;
extern StatTable globalStats;

#endif
