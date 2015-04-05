/* Implemented arguments:
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
 * -I <n>:  Profile-assisted search max of <n> iterations, but hopefully fewer
 * 
 * -c <n>: number of cores, default is 4
 *
 * -o <n>: the initial overhead for a new thread in number of reductions.
 *         default is 0
 *
 * -h <n>: The number of heap cells in thousands (defaults to 10,000 [i.e. 10,000,000 cells])
 *
 * -L <filename>:  specified log file
 * 
 */
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
#include "lib/sgetopt.h"
//#include "lex.yy.c"
#define TIME_SLICE 5
#define DEFHEAPSIZE 10000000

/* Ugly globals for stats and logging of stats */
unsigned int threadCounter;
unsigned long long int globalReductions;
unsigned long long int totalBlocked;
unsigned int evalPrintLoop;
//FILE *logThreadsFile, *logProgFile;
char* logFileNameP;
char* logFileNameT;
StatTable globalStats;
unsigned int NUM_CORES;

instruction *gprog;
int progLeng;
Machine *seqMach;

void initMachine(Machine *mach, unsigned int iOH, unsigned int cOH) {
    mach->progCounter  = NULL;
    mach->status  = NEW;
    mach->stck = initStack(mach->stck);

    /* TODO profiling only */
    mach->reductionCounter = 0;
    mach->blockedCounter = 0;
    mach->threadID = threadCounter;
    threadCounter += 1;
    mach->birthTime = globalReductions;
    mach->initOH = iOH;
    mach->creationOH = cOH;
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

    resStr = malloc(strlen(gcodeFileName) + 5); //The + 4 is to ensure there is enough space for .log
    if (resStr == NULL) {
        return NULL;
    }
    strcpy(resStr, gcodeFileName);
    lastDot = strrchr(resStr, '.');
    if (lastDot == NULL) {
        strcat(resStr, ".log");
    }
    else {
        strcpy(lastDot, ".log");
    }
    return resStr;
}

int capture_search(const char *arg, void *pvar) {

    printf("Testing: %c\n", *arg);
    return 0;

}

int main(int argc, char* argv[]) {

    unsigned long int infSearch = 0;
    unsigned long int hillSearch = 0;
    unsigned long int randSearch = 0;
    unsigned long int iFlag = 0;
    enum searchTypes_ sType = NONE;
    int seqRun = 0;
    int cFlag = 0;       /* Flag for number of cores */
    int lFlag = 0;       /* logfile name */
    int seed = -1;        /* random seed */
    char *logFileName = NULL;
    unsigned long int hSize = 0;       /* Size of heap (in thousands) */
    unsigned long int initOverhead = 0;
    unsigned long int createOverhead = 0;
    opterr = 0;          /* don't show error for no CLI args */
    int fnIndex;


    struct soption opttable[] = {
            { 'h', "heap",          1, capture_unsigned_int, &hSize },
            { 'o', "init-overhead", 1, capture_unsigned_int, &initOverhead },
            { 'O', "overhead",      1, capture_unsigned_int, &createOverhead },
            { 'I', "inf-search",    1, capture_unsigned_int, &infSearch },
            { 'R', "rand-search",   1, capture_unsigned_int, &randSearch },
            { 'H', "hill-climb",    1, capture_unsigned_int, &hillSearch },
            { 'S', "sequential",    0, capture_presence,     &seqRun },
            { 'c', "cores",         1, capture_int,          &cFlag },
            { 's', "seed",          1, capture_int,          &seed },
            { 0,   0,               0, 0,                    0 }
       };

    /* argv+1 will point to all non-option arguments */
    if (sgetopt(argc, argv, opttable, argv+1, 0)) {
        printf("Error parsing one of the command line options\n");
        return 1;
    }

    /* set the search type and max iterations */
    if (infSearch) {
        sType = ITER;
        iFlag = infSearch;
    } else if (randSearch) {
        sType = RAND;
        iFlag = randSearch;
    } else if (hillSearch) {
        sType = HILL;
        iFlag = hillSearch;
    } else if (seqRun) {
        sType = NONE_SEQ;
        iFlag = 1;
    }

    if (!iFlag)
        iFlag = 1;

    if (!cFlag) {
        NUM_CORES = 4;
    }

    srand(seed < 0 ? time(0) : seed);
    printf("Using random seed: %d\n", seed);

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
    /* We want two logging files, one for the individual thread statistics and
     * and one for the overal program info
     */
    if (!lFlag)
        logFileName = getLogFileName(argv[fnIndex]);

    logFileNameP = malloc(strlen(logFileName) + 10);
    logFileNameT = malloc(strlen(logFileName) + 10);
    strcpy(logFileNameP, logFileName);
    strcat(logFileNameP, ".plog");
    strcpy(logFileNameT, logFileName);
    strcat(logFileNameT, ".tlog");
    globalStats.lpName = logFileNameP;
    globalStats.ltName = logFileNameT;
    //logThreadsFile = fopen(logFileNameT, "w");
    //logProgFile = fopen(logFileNameP, "w");


    free(logFileName);

    //setup `passing by reference' for our switches
    parSwitch* switches = malloc(sizeof(parSwitch));
    switches->address = -1;
    parSwitch** switchesPtr = &switches;

    //parse the actual GCode
    prog = parseGCode(inputFile, switchesPtr);
    gprog = prog;

    int plen = 4;
    while(prog[plen].type != End) {
        plen++;
    }
    progLeng = plen;

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
        printf("Max number of interations: %lu\n", iFlag);


    fclose(inputFile);


    // Allocate and initialize the heap (double needed space since it's Cheney's
    // GC)
    if (!hSize)
        hSize = DEFHEAPSIZE;

    printf("Size of heap: %lu\n", hSize);

    globalHeap = malloc(sizeof(Heap));
    globalHeap->gcs = 0;
    globalHeap->maxSize = hSize;
    globalHeap->toSpace = malloc(sizeof(HeapCell) * hSize);
    globalHeap->fromSpace = malloc(sizeof(HeapCell) * hSize);

    globalPool = malloc(sizeof(threadPool));
    globalPool->initOH = initOverhead;
    globalPool->createOH = createOverhead;

    int numPar = counter;

    iterate(switches, numPar, &globalStats, sType, iFlag, prog);

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
        if (swtchs[i].stratPar == TRUE) {
            if (swtchs[i].pswitch == 1) {
                strcpy(prog[swtchs[i].address].pushGlobVal, "parStrat");
            } else if (swtchs[i].pswitch == 0) {
                strcpy(prog[swtchs[i].address].pushGlobVal, "seq");
            }
        } else {
            if (swtchs[i].pswitch == 1) {
                strcpy(prog[swtchs[i].address].pushGlobVal, "par");
            } else if (swtchs[i].pswitch == 0) {
                strcpy(prog[swtchs[i].address].pushGlobVal, "parOff");
            }
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
    initMachine(cores[0], 0, globalPool->createOH);
    seqMach = *cores;
    cores[0]->creatorID = 0;
    cores[0]->parSite = prog;
    cores[0]->progCounter = prog;
    cores[0]->status = RUNNING;

    /* Initialize the stat table
     * TODO make this dependent on profiling flag
     */
    initTable(prog, 300, logFileNameT, logFileNameP, &globalStats);

    Machine* fromThreadPool = NULL;

    fprintf(globalStats.lp, "#GRC\tLiveCores\n");

    while (programMode == LIVE) {
        int j;
        int numLiveCores;
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

            numLiveCores = 0;
            for (i = 0; i < NUM_CORES; i++) {

                core = UNKNOWN;
                if (cores[i] != NULL) {
                    numLiveCores += 1;
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
            fprintf(globalStats.lp, "%llu\t%d\n", globalReductions, numLiveCores);
        }

        for (i = 0; i < NUM_CORES; i++) {
            if (cores[i] != NULL) {
                addMachToThreadPool(cores[i], globalPool);
                cores[i] = NULL;
            }
        }
    }

    printf("\nTotal Reductions: %llu\n", globalReductions);
    printf("Garbage Collections: %d\n", globalHeap->gcs);


   free(cores);

   return globalReductions;

}

ExecutionMode dispatchGCode(Machine *mach) {
    if (mach == NULL) {
        return FINISHED;
    }
    ExecutionMode em = LIVE;

    /* If we have an overhead set, the reduction doesn't do
     * anything, but is still taking up an 'CPU' slot
     */
    if (mach->initOH > 0) {
        mach->initOH -= 1;
        return em;
    }

    instruction *oldPC = mach->progCounter;
    mach->progCounter += 1;

    
    /* Run the appropriate GInstruction.
     * A more performant version would eliminate
     * the function calls and have the body of each function here
     */
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
