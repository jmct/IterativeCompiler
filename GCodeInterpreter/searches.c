#include <stdlib.h>
#include <stdio.h>
#include "machine.h"
#include "stats.h"
#include "instruction_type.h"
#include "searches.h"

void prettyPrSwitches(parSwitch* switches, int nSwitch) {
    fputs("Bitstring used: ", stdout);
    int i;
    for (i = 0; i < nSwitch; i++) {
        printf("%d", switches[i].pswitch);
    }
    puts("\n");
}

int * mkSArray(parSwitch* swtchs, int nSwitch) {
    int * sArray = malloc(sizeof(int) * nSwitch); 
    int i;
    for (i = 0; i < nSwitch; i++) {
        sArray[i] = swtchs[i].pswitch;
    }

    return sArray;
}

void swtchToArray(parSwitch* swtchs, int nSwitch, int* sArray) {
    int i;
    for (i = 0; i < nSwitch; i++) {
        sArray[i] = swtchs[i].pswitch;
    }
}
    

typedef struct {
    int *swtchs;
    unsigned int rCount;
} elite;

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
        elite best;
        best.swtchs = mkSArray(switches, nSwitch);
        unsigned int gr;
        int i;
        for (i = 1; i <= maxI; i++) {
            gr = 0;
            gr = executeProg(switches, prog, nSwitch);
            if (i == 1) {
                best.rCount = gr;
            }
            else if (gr < best.rCount) {
                swtchToArray(switches, nSwitch, best.swtchs);
                best.rCount = gr;
            }

            printf("Run %d took %u reductions\n", i, gr);
            prettyPrSwitches(switches, nSwitch);
            randMutate(switches, nSwitch);
        }
        
        fputs("Best performing configuration: ", stdout);
        for (i = 0; i < nSwitch; i++) {
            printf("%d", best.swtchs[i]);
        }
        printf(" taking %u reductions\n", best.rCount);
    }

    /* This shouldn't happen */
    return NULL;
}
