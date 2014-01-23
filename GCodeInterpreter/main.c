#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <gsl/gsl_math.h>
#include "ginstructions.h"
#include "instruction_type.h"
#include "symbolTable.h"
#include "heap.h"
#include "stack.h"
#include "gthread.h"
#include "machine.h"
#include "stats.h"
//#include "lex.yy.c"
#define NUM_CORES 4
#define TIME_SLICE 5
#define HEAPSIZE 10000000

/* Ugly globals for stats and logging of stats */
unsigned int threadCounter;
unsigned int globalReductions;
FILE* logFile;
StatTable globalStats;

void initMachine(Machine *mach) {
    mach->progCounter  = NULL;
    mach->status  = NEW;
    mach->stck = initStack(mach->stck);

    /* TODO profiling only */
    mach->reductionCounter = 0;
    mach->blockedCounter = 0;
    mach->threadID = threadCounter;
    threadCounter += 1;
    mach->birthTime = globalReductions;
}

/* TODO when freeing a machine we need to keep the statistics */
void freeMachine(Machine* mach) {
   freeStack(mach->stck);
   
   unsigned int lifespan = globalReductions - mach->birthTime;
   recordMach(mach, &globalStats, lifespan);
   free(mach);
}


//Heap* globalHeap = NULL;
threadPool* globalPool = NULL;

/* Function prototype */
ExecutionMode dispatchGCode(Machine *mach);

char * getLogFileName(char * gcodeFileName) {
    char* resStr;
    char* lastDot;

    resStr = malloc(strlen(gcodeFileName) + 4); //The + 4 is to ensure there is enough space for .log
    if (resStr == NULL) {
        return NULL;
    }
    strcpy(resStr, gcodeFileName);
    lastDot = strrchr(resStr, '.');
    if (lastDot == NULL) {
        strcat(resStr, ".log");
    }
    else {
        strcpy(lastDot, ".log\0");
    }
    return resStr;
}


int main(int argc, char* argv[]) {
    if (argc < 2) {
        printf("No GCode file specified\n\nUsage: %s <filename>\n", argv[0]);
        exit (1);
    }
    instruction *prog = NULL;

    //open GCode file. Right now we ignore any additional arguments
    FILE * inputFile = fopen(argv[1], "r");
    if (inputFile == 0) {
        printf("Unable to open input file :(\n");
        exit (1);
    }
    char* logFileName = getLogFileName(argv[1]);
    logFile = fopen(logFileName, "w");

    free(logFileName);
    
    //setup `passing by reference' for our switches
    parSwitch* switches = malloc(sizeof(parSwitch));
    parSwitch** switchesPtr = &switches;

    //parse the actual GCode
    prog = parseGCode(inputFile, switchesPtr);
    //get the value back from switchesPtr
    switches = *switchesPtr;

    int counter = 0;
    do {
        if (switches[counter].address < 0) {
            counter *= -1;
        }
        else {
            counter++;
        }
    } while (counter > 0);
    /* Set the counter back to it's abs value */
    counter *= -1;
    printf("There are %d par sites in the program\n", counter);


    fclose(inputFile);


    // Allocate and initialize the heap (double needed space since it's Cheney's
    // GC)
    globalHeap = malloc(sizeof(Heap));
    globalHeap->nextFreeCell = 0;
    globalHeap->maxSize = HEAPSIZE;
    globalHeap->toSpace = malloc(sizeof(HeapCell) * HEAPSIZE);
    globalHeap->fromSpace = malloc(sizeof(HeapCell) * HEAPSIZE);

    //allocate and intialize thread pool
    globalPool = malloc(sizeof(threadPool));
    initThreadPool(globalPool);

    //allocate and initlialize Machines
    ExecutionMode programMode, core;
    programMode = core = LIVE;
    Machine** cores = malloc(sizeof(Machine*) * NUM_CORES);
    int i;
    for (i = 0; i < NUM_CORES; i++) {
        cores[i] = NULL;
    }
    threadCounter = 1;
    globalReductions = 0;
    cores[0] = malloc(sizeof(Machine));
    initMachine(cores[0]);
    cores[0]->creatorID = 0;
    cores[0]->parSite = prog;
    cores[0]->progCounter = prog;
    cores[0]->status = RUNNING;
    //set roots for heap
    globalHeap->activeCores = cores;
    globalHeap->numCores = NUM_CORES;
    globalHeap->thrdPool = globalPool;


    /* Initialize the stat table
     * TODO make this dependent on profiling flag 
     */
    initTable(prog, 300, &globalStats);
    
    Machine* fromThreadPool = NULL;

    while (programMode == LIVE) {
        int j;
        for (j = 0; j <= TIME_SLICE; j++) {
            globalReductions += 1;
            programMode = FINISHED;

            /* Any null cores need to be replaced */
            for (i = 0; i < NUM_CORES; i++) {
                fromThreadPool = NULL;
                //see if the core needs to pull from the spark pool
                if (cores[i] == NULL) {
                    fromThreadPool = getMachFromPool(globalPool);
                    if (fromThreadPool != NULL) {
                        cores[i] = fromThreadPool;
                        programMode = LIVE;
                    }
                }
                else {
                    programMode = LIVE;
                }
            }
            if (programMode == FINISHED)
                break;

            for (i = 0; i < NUM_CORES; i++) {

                core = UNKNOWN;
                if (cores[i] != NULL) {
                    if (cores[i]->status == NEW) {
                        cores[i]->status = RUNNING;
                        eval(cores[i]);
                        core = unwind(cores[i]);
                    }
                    else if (cores[i]->status == WAS_BLOCKED) {
                        cores[i]->status = RUNNING;
                        core = unwind(cores[i]);
                    }
                    else {
                        core = dispatchGCode(cores[i]);
                    }
                    cores[i]->reductionCounter += 1;

                }

                if (core == FINISHED) {
                    cores[i]->reductionCounter += 1;
                    freeMachine(cores[i]);
                }
                if (core != LIVE) {
                    cores[i] = NULL;
                }
            }
        }

        for (i = 0; i < NUM_CORES; i++) {
            if (cores[i] != NULL) {
                addMachToThreadPool(cores[i], globalPool);
                cores[i] = NULL;
            }
        }
    }

    printf("\nTotal Reductions: %d\n", globalReductions);

    /* write statTable to log file */
    /*TODO make this dependent on profiling flag */
    globalStats.entries = realloc(globalStats.entries, 
                                  sizeof(StatRecord) * globalStats.currentEntry);
    if (globalStats.entries == NULL) {
        printf("Resizing stats table failed after run\n");
    }
    else {
        qsort(globalStats.entries, globalStats.currentEntry, 
              sizeof(StatRecord), compare_entries);
        int nStats = logStats(&globalStats, logFile);
        printf("Recorded %d threads\n", nStats);

        /* parSite Stats */
        /* The (+1) for the number of par sites is due to the fact that the main
         * thread doesn't need a par site to spark it
         */
        ParSiteStats * psStats = calcParSiteStats(&globalStats, counter + 1);
        printf("Testing Par site stats: %f\n", psStats[0].rcMean);
        
    }

    fclose(logFile);
    return 0;
}

ExecutionMode dispatchGCode(Machine *mach) {
    if (mach == NULL) {
        return FINISHED;
    }
    ExecutionMode em = LIVE;
    instruction *oldPC = mach->progCounter;
    mach->progCounter += 1;
    switch (oldPC->type) {
        case Unwind:
            em = unwind(mach);
            break;
        case PushGlobal:
            pushGlobal(oldPC, mach);
            break;
        case PushInt:
            pushInt(oldPC->pushIntVal, mach);
            break;
        case Push:
            push(oldPC->pushVal, mach);
            break;
        case MkAp:
            mkAp(mach);
            break;
        case Update:
            update(oldPC->updateVal, mach);
            break;
        case Pop:
            pop(oldPC->popVal, mach);
            break;
        case Slide:
            slide(oldPC->slideVal, mach);
            break;
        case Alloc:
            alloc(oldPC->allocVal, mach);
            break;
        case Eval:
            eval(mach);
            em = unwind(mach);
            break;
        case Add:
            addI(mach);
            break;
        case Sub:
            subI(mach);
            break;
        case Mul:
            mulI(mach);
            break;
        case Div:
            divI(mach);
            break;
        case Neg:
            negI(mach);
            break;
        case Eq:
            eqI(mach);
            break;
        case Ne:
            neI(mach);
            break;
        case Lt:
            ltI(mach);
            break;
        case Le:
            leI(mach);
            break;
        case Gt:
            gtI(mach);
            break;
        case Ge:
            geI(mach);
            break;
        case Pack:
            pack(oldPC->packVals.tag, oldPC->packVals.arity, mach);
            break;
        case CaseJump:
            casejump(oldPC->labelVal, mach);
            break;
        case CaseAlt: //<<<<<<<<< Maybe moved to Default case?
            break;
        case CaseAltEnd:
            //Here we need to append "EndCase" to the labelVal and 
            //jump to the result of a lookup
            caseAltEnd(oldPC->labelVal, mach);
            break;
        case Split:
            split(oldPC->splitVal, mach);
            break;
        case GLabel: //<<<<<<<<< Maybe moved to Default case?!
            break;
        case FunDef: //<<<<<<<<< Maybe moved to Default case?!
            em = FINISHED;
            //printf("WE HAVE REACHED A FUNDEF AND FINISHED THREAD");
            break;
        case Print:
            printI(mach);
            break;
        case Par:
            parI(mach, globalPool);
            //printf("Trying Par");
            break;
        default:
            break;
    }
    return em;
}
