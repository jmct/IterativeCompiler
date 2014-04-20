/* This file contains all of the implemented search strategies for the
 * iterative compiler. There is only one exported function (via searches.h),
 * iterate(). The rest of the file is internal. The user specifies which search
 * strategy they want on the comman line. So far we have the following flags
 * implemented:
 *
 * no args: standard interpretation. Run the program will all the annotated
 *          parallelism.
 *
 * -S:  Ignore any par annotations and run the program sequentially.
 *
 * -R <n>: Random search -> randomly mutate the bitstring and execute the
 *         corresponding program. Do this <n> times and display the best result.
 *
 * -H <n>:  Standard Hill-Climbing with <n> climbs
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include "machine.h"
#include "stats.h"
#include "instruction_type.h"
#include "searches.h"

/* When we want to use elitism in a search algorithm */
typedef struct {
    int *swtchs;
    unsigned int rCount;
} elite;


void prettyPrSwitches(parSwitch *switches, int nSwitch);

int * mkSArray(parSwitch *swtchs, int nSwitch);

void swtchToArray(parSwitch *swtchs, int nSwitch, int *sArray);

void randMutate(parSwitch *switches, int nSwitch);

unsigned int randSearch(parSwitch *switches, int nSwitch, int maxI, instruction *prog);

unsigned int hillClimb(parSwitch *switches, int nSwitch, int maxI, instruction *prog);

parSwitch* iterate(parSwitch *switches, int nSwitch, StatTable *gStat,
                    enum searchTypes_ sType, int maxI, instruction *prog)
{

    unsigned int rCount;

    if (sType == NONE || sType == NONE_SEQ) {
        executeProg(switches, prog, nSwitch);
        return switches;
    } else if (sType == RAND) {
        rCount = randSearch(switches, nSwitch, maxI, prog);
    } else if (sType == HILL) {
        rCount = hillClimb(switches, nSwitch, maxI, prog);
    }

    fputs("Best performing configuration: ", stdout);
    int i;
    for (i = 0; i < nSwitch; i++) {
        printf("%d", switches[i].pswitch);
    }
    printf(" taking %u reductions\n", rCount);

    /* This shouldn't happen */
    return NULL;
}

void prettyPrSwitches(parSwitch *switches, int nSwitch) {
    fputs("Bitstring used: ", stdout);
    int i;
    for (i = 0; i < nSwitch; i++) {
        printf("%d", switches[i].pswitch);
    }
    puts("\n");
}

int *mkSArray(parSwitch* swtchs, int nSwitch) {
    int *sArray = malloc(sizeof(int) * nSwitch);
    int i;
    for (i = 0; i < nSwitch; i++) {
        sArray[i] = swtchs[i].pswitch;
    }

    return sArray;
}

void swtchToArray(parSwitch *swtchs, int nSwitch, int *sArray) {
    int i;
    for (i = 0; i < nSwitch; i++) {
        sArray[i] = swtchs[i].pswitch;
    }
}

void arrayToSwtch(int *sArray, int nSwitch, parSwitch *swtchs) {
    int i;
    for (i = 0; i < nSwitch; i++) {
        swtchs[i].pswitch = sArray[i];
    }
}

void cpSArray(int *from, int *to, int n) {
    int i;
    for (i = 0; i < n; i++) {
        to[i] = from[i];
    }
    return;
}

/* Random search starts from the initial switch setting (all 1s) and jumps to
 * random points in the search space. We save the best performing setting
 * throughout the process */
unsigned int randSearch(parSwitch *s, int nSwitch, int maxI, instruction *prog)
{
    unsigned int gr = UINT_MAX;

    elite best;
    best.rCount = gr;
    best.swtchs = mkSArray(s, nSwitch);

    int i;
    for (i = 1; i <= maxI; i++) {
        randMutate(s, nSwitch);

        gr = executeProg(s, prog, nSwitch);

        if (gr < best.rCount) {
            swtchToArray(s, nSwitch, best.swtchs);
            best.rCount = gr;
        }

        printf("Run %d took %u reductions\n", i, gr);
        prettyPrSwitches(s, nSwitch);

    }
    arrayToSwtch(best.swtchs, nSwitch, s);
    free(best.swtchs);

    return best.rCount;
}



void randMutate(parSwitch* switches, int nSwitch) {
    int i;
    for (i = 0; i < nSwitch; i++) {
        switches[i].pswitch = rand()%2;
    }
    return;
}

int sArrayCmp(int *fst, int *snd, int n) {
    int i;
    for (i = 0; i < n; i++) {
        if (fst[i] != snd[i])
            return 0;
    }

    return 1;
}

int hasBeenTried(int attempt, int *tried, int nTried)
{
    int i;
    for (i = 0; i < nTried; i++) {
        if (tried[i] == attempt)
            return 1;
    }

    return 0;
}

/* For hill climbing there are a few approaches. TODO: Copy Simon's hierarchy
 * into the repo so that we can refer to it here.
 *
 * The approach used in this function is the simplest form:
 *
 * 1) Start from a random point, A.
 * 2) Compute a random neighbor of A, B.
 * 3) If the neighbor performs better than A, then B is your new point.
 * 4) Iterate from 2 again until max number of iterations reached.
 */
unsigned int hillClimb(parSwitch *swtchs, int nSwitch, int maxI, instruction *prog)
{
    int triedNbrs[nSwitch];
    int attempt, nTried = 0;
    /* Get random starting point */
    randMutate(swtchs, nSwitch);

    elite best;
    best.rCount = executeProg(swtchs, prog, nSwitch);
    best.swtchs = mkSArray(swtchs, nSwitch);

    int i, j;
    unsigned int curRed;

    for (i = 0; i < maxI; i++) {

        /* We keep track of which neighbors have
         * been tried so far and avoid those
         * The current idea is to use a stack allocated
         * array that just records which indices have been
         * mutated */
        attempt = rand()%nSwitch;
        for (j = 0; j < nSwitch; j++) {
            while (hasBeenTried(attempt, triedNbrs, nTried))
                attempt = rand()%nSwitch;

            triedNbrs[j] = attempt;
            nTried++;

            if (swtchs[attempt].pswitch == TRUE)
                swtchs[attempt].pswitch = FALSE;
            else
                swtchs[attempt].pswitch = TRUE;

            curRed = executeProg(swtchs, prog, nSwitch);

            /* If a better candidate is found, we set that as the new
             * candidate and start again */
            if (curRed < best.rCount) {
                best.rCount = curRed;
                swtchToArray(swtchs, nSwitch, best.swtchs);
                break;
            }
        }

        nTried = 0;

        /*
            else if (sType == HILL) {
                hillClimb(switches, nSwitch, gr);
            }
         */
    }
    arrayToSwtch(best.swtchs, nSwitch, swtchs);
    free(best.swtchs);

    return best.rCount;
}


