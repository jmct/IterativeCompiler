#include "machine.h"
#include "stats.h"
#include "instruction_type.h"
#include "searches.h"

parSwitch* randMutate(parSwitch* switches, int nSwitch) {
    /* TODO: This function */
    return NULL;
}


parSwitch* iterate(parSwitch* switches, int nSwitch, StatTable* gStat, enum searchTypes_ sType, int maxI, instruction* prog) {
    if (sType == NONE) {
        executeProg(switches, prog, nSwitch);
        return switches;
    }
    else if (sType == RAND) {
        unsigned int gr;
        int i;
        for (i = 1; i <= maxI; i++) {
            gr = 0;
            gr = executeProg(switches, prog, nSwitch);
            randMutate(switches, nSwitch);
            printf("Run %d took %u reductions\n", i, gr);
        }

    }

    /* This shouldn't happen */
    return NULL;
}
