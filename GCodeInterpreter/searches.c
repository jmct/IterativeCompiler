#include <stdlib.h>
#include <stdio.h>
#include "machine.h"
#include "stats.h"
#include "instruction_type.h"
#include "searches.h"

void prettyPrSwitches(parSwitch* switches, int nSwitch) {
    puts("Bitstring used: ");
    int i;
    for (i = 0; i < nSwitch; i++) {
        printf("%d", switches[i].pswitch);
    }
    puts("\n");
}

/*
int * makeIntArray(parSwitch *switches, 
*/

void randMutate(parSwitch* switches, int nSwitch) {
    int i;
    for (i = 0; i < nSwitch; i++) {
        switches[i].pswitch = rand()%2;
    }
    return;
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
            printf("Run %d took %u reductions\n", i, gr);
            prettyPrSwitches(switches, nSwitch);
            randMutate(switches, nSwitch);
        }

    }

    /* This shouldn't happen */
    return NULL;
}
