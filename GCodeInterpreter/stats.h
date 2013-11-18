#ifndef STATS_HEADER
#define STATS_HEADER
#include "machine.h"

/* The structures below are only used when profiling
 * TODO make these definitions depend on a 'profiling' flag
 */
struct _StatRecord {
    unsigned int threadID, reductionCounter, lifespan;
    unsigned int blockedCouter, aliveTime, creatorID;
};

typedef struct _StatRecord StatRecord;

struct _StatTable {
    unsigned int size, currentEntry;
    StatRecord * entries;
};

typedef struct _StatTable StatTable;

/*Profiling stat funtions */
int recordMach(Machine* mach, StatTable* table, 
              FILE* logFile, unsigned int lifepsan);

#endif
