#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <ctype.h>
#include <gsl/gsl_math.h>
#include <time.h>
#include "ginstructions.h"
#include "instruction_type.h"
#include "symbolTable.h"
#include "heap.h"
#include "stack.h"
#include "gthread.h"
#include "machine.h"
#include "stats.h"
#include "searches.h"
//#include "lex.yy.c"
#define TIME_SLICE 5
#define HEAPSIZE 10000000

/* Ugly globals for stats and logging of stats */
unsigned int threadCounter;
unsigned int globalReductions;
unsigned int evalPrintLoop;
FILE* logFile;
StatTable globalStats;
unsigned int NUM_CORES;

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


Heap* globalHeap = NULL;
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

    char* iVal = NULL;   /* Max number of iterations for compiler */
    int iFlag = 0;       /* Flag for max iterations */
    int cFlag = 0;       /* Flag for number of cores */
    enum searchTypes_ sType = NONE;
    opterr = 0;          /* don't show error for no CLI args */
    int fnIndex;

    int c = 0;               /* char for parsing CLI args */

    srand(time(NULL));

    /* Parse CLI args */
    while ((c = getopt(argc, argv, "c:SI:R:H:")) != -1) {
        switch (c)
         {
         case 'c':
            cFlag = 1;
            NUM_CORES = atoi(optarg);
            break;
         case 'S':
            sType = NONE_SEQ;
            break;
         case 'I':
            iFlag = 1;
            sType = ITER;
            iVal = optarg;
            break;
         case 'H':
            iFlag = 1;
            sType = HILL;
            iVal = optarg;
            break;
         case 'R':
            iFlag = 1;
            sType = RAND;
            iVal = optarg;
            break;
         case '?':
            if (optopt == 'I')
              fprintf (stderr, "Option -%c requires an integer argument.\n", optopt);
            else if (isprint (optopt))
              fprintf (stderr, "Unknown option `-%c'.\n", optopt);
            else
              fprintf (stderr, "Unknown option character `\\x%x'.\n", optopt);
              return 1;
         }
    }

    if (!cFlag)
        NUM_CORES = 4;

    fnIndex = optind;    /* After parsing options, the renaming args will be at optind */



    if (argc < 2) {
        printf("No GCode file specified\n\nUsage: %s <filename>\n", argv[0]);
        exit (1);
    }
    instruction *prog = NULL;

    //open GCode file. Right now we ignore any additional arguments
    FILE * inputFile = fopen(argv[fnIndex], "r");
    if (inputFile == 0) {
        printf("Unable to open input file :(\n");
        exit (1);
    }
    char* logFileName = getLogFileName(argv[fnIndex]);
    logFile = fopen(logFileName, "w");

    free(logFileName);

    if (iFlag) {
        iFlag = atoi(iVal);
    }
    else {
        iFlag = 1;
    }

    //setup `passing by reference' for our switches
    parSwitch* switches = malloc(sizeof(parSwitch));
    switches->address = -1;
    parSwitch** switchesPtr = &switches;

    //parse the actual GCode
    prog = parseGCode(inputFile, switchesPtr);
    //get the value back from switchesPtr
    switches = *switchesPtr;

    unsigned int counter;

    for (counter = 0; switches[counter].address > 0; counter++);

    printf("There are %d par sites in the program\nUsing %u cores\n", counter, NUM_CORES);
    
    if (counter == 0)
        sType = NONE;

    char* searchName;
    if (sType == RAND) {
        searchName = "Random Search\n";
    }
    else if (sType == HILL) {
        searchName = "Hill Climbing\n";
    }
    else if (sType == ITER) {
        searchName = "Informed Search\n";
    }
    else if (sType == NONE_SEQ) {
        for (counter = 0; switches[counter].address > 0; counter++) {
            switches[counter].pswitch = FALSE;
        }
        puts("Ignoring par annotations and running sequentially.");
    }

    if ((sType != NONE) && (sType != NONE_SEQ))
        printf("Performing search using: %s", searchName);

    if (iFlag)
        printf("Max number of interations: %d\n", iFlag);


    fclose(inputFile);


    // Allocate and initialize the heap (double needed space since it's Cheney's
    // GC)
    globalHeap = malloc(sizeof(Heap));
    globalHeap->maxSize = HEAPSIZE;
    globalHeap->toSpace = malloc(sizeof(HeapCell) * HEAPSIZE);
    globalHeap->fromSpace = malloc(sizeof(HeapCell) * HEAPSIZE);

    globalPool = malloc(sizeof(threadPool));

    int numPar = counter;

    iterate(switches, numPar, &globalStats, sType, iFlag, prog);

   fclose(logFile);

   return 0;
}

unsigned int executeProg(parSwitch* swtchs, instruction* prog, int counter) {
    /*  Initialize:
        Heap
        Thread Pool
        Cores
        Stat table?
     */
    //allocate thread pool

    /* use switches */
    int i;
    for (i = 0; i < counter; i++) {
        if (swtchs[i].pswitch == 1) {
            strcpy(prog[swtchs[i].address].pushGlobVal, "par");
        } else if (swtchs[i].pswitch == 0) {
            strcpy(prog[swtchs[i].address].pushGlobVal, "parOff");
        }
    }


    //allocate and initlialize Machines
    ExecutionMode programMode, core;
    Machine** cores = malloc(sizeof(Machine*) * NUM_CORES);
    for (i = 0; i < NUM_CORES; i++) {
        cores[i] = NULL;
    }

    //set roots for heap
    globalHeap->activeCores = cores;
    globalHeap->numCores = NUM_CORES;
    globalHeap->thrdPool = globalPool;

    puts("\n");

    evalPrintLoop = 1;

    globalHeap->nextFreeCell = 0;
    initThreadPool(globalPool);

    programMode = core = LIVE;

    threadCounter = 1;
    globalReductions = 0;

    cores[0] = malloc(sizeof(Machine));
    initMachine(cores[0]);
    cores[0]->creatorID = 0;
    cores[0]->parSite = prog;
    cores[0]->progCounter = prog;
    cores[0]->status = RUNNING;

    /* Initialize the stat table
     * TODO make this dependent on profiling flag
     */
    initTable(prog, 300, logFile, &globalStats);

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


   free(cores);

   return globalReductions;

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
