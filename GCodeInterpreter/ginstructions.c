#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "ginstructions.h"
#include "instruction_type.h"
#include "symbolTable.h"
#include "heap.h"
#include "stack.h"
#include "gthread.h"
#include "machine.h"
#include "stats.h"


//This is for the GCode instruction 'Slide n'
void slideNStack(int n, Machine *mach) {
    HeapCell *temp = stackPopKeep(&mach->stck);
    popNFromStack(n, &mach->stck);
    stackPush(temp, &mach->stck);
}


//Any top-level function will be pushed onto to the stack
//via this function
void pushGlobal(instruction *fun, Machine *mach) {
    instruction* codePtr = lookupKey(fun->pushGlobVal);
    /* the reason we do not add 1 to codePtr above is so that we can get the
     * arity information from the function definition instruction.
     * this type of information could be added to the symbol table to more
     * efficiency. (So the symbol table wouldn't just store addresses but structs that
     * had all relevant information based on the tag of the result of the lookup    
     */

    HeapPtr addr = allocFun(codePtr->funVals.arity, codePtr, globalHeap);
    //don't do effectful expressions in parameter list! adding 1 to codePtr can break 
    //lookup for first parameter. 

    /* For keeping stats on how created this node */
    if (strcmp(fun->pushGlobVal, "par") == 0) {
        addr->parSite = (mach->progCounter - 1);
        assert(strcmp(addr->parSite->pushGlobVal, "par") == 0);
    }

    addr->creatorID = mach->threadID;

    stackPush(addr, &mach->stck);
}

void pushInt(int val, Machine * mach) {
    HeapPtr addr = allocInt(val, globalHeap);
    stackPush(addr, &mach->stck);
}

//MkAp simply takes the two topmost items on the stack
//and replaces them with an application node pointing 
//to both
void mkAp(Machine *mach) {
    HeapCell *leftArg, *rightArg, *newNode;
    leftArg = stackPopKeep(&mach->stck);
    rightArg = stackPopKeep(&mach->stck);
    newNode = allocApp(leftArg, rightArg, globalHeap);
    stackPush(newNode, &mach->stck);
}

//Push the value located n elements from top of stack
void push(int offset, Machine *mach) {
    HeapCell* addr = getNthElement(offset, &mach->stck);
    stackPush(addr, &mach->stck);
}

//slide the top of the stack N places
//i.e. popAndKeep then move the SP back n spaces then push what you kept
void slide(int num, Machine *mach) {
    HeapCell **newSP = NULL;
    HeapCell * top = stackPopKeep(&mach->stck);
    newSP = getNthAddrFromSP(num, &mach->stck);
    mach->stck.stackPointer = newSP;
    stackPush(top, &mach->stck);
}

//Pop the top N elements off the stack
void pop(int num, Machine *mach) {
    HeapCell **newSP = NULL;
    newSP = getNthAddrFromSP(num, &mach->stck);
    mach->stck.stackPointer = newSP;
}

void unlock(HeapPtr node, int newTag) {
    while (node->tag == LOCKED_APP) {
        //TODONE empty pending list <--I think it's done
        if (node->app.numBlockedThreads > 0) {
            /* There are two versions of addQueueToThreadPool
             * TODO make which one is used switchable by profiling flag
             * The one that is currently commented out is for non-profiled
             * builds
             *
            addQueueToThreadPool(node->app.blockedQueue, node->app.numBlockedThreads, globalPool);
             */
            addQueueToThreadPoolProf(node->app.blockedQueue, node->app.numBlockedThreads, globalPool,
                                 globalReductions);
            node->app.numBlockedThreads = 0;
            node->app.blockedQueue = NULL;
        }
        node->tag = newTag;
        node = node->app.leftArg;
    }
    if (node->tag == LOCKED_FUN) {
        if (node->fun.numBlockedThreads > 0) {
            /* There are two versions of addQueueToThreadPool
             * TODO make which one is used switchable by profiling flag
             * The one that is currently commented out is for non-profiled
             * builds
             *
            addQueueToThreadPool(node->app.blockedQueue, node->app.numBlockedThreads, globalPool);
             */
            addQueueToThreadPoolProf(node->app.blockedQueue, node->app.numBlockedThreads, globalPool,
                                 globalReductions);
            node->fun.numBlockedThreads = 0;
            node->fun.blockedQueue = NULL;
        }
        //TODONE empty pending list <--I think it's done
        node->tag = FUN;
    }
    else {
        return;
    }
}
        
        
//update the pointer to the top of the expression tree to point
//to an indirection node (this allows for sharing)
//TODONE:
//When locked nodes introduced, this function must take them into accont
void update(int num, Machine *mach) {
    HeapCell **toUpdate = NULL;
    HeapCell * top = stackPopKeep(&mach->stck);
    toUpdate = getNthAddrFrom(num, &mach->stck, mach->stck.stackPointer);
    if (*toUpdate == NULL) {
        printf("\nItems in frame: %d, update parameter val: %d\n", itemsInFrame(&mach->stck), num);
    }
    unlock(*toUpdate, APP);
    HeapCell * newNode = updateToInd(top, *toUpdate);
    //*toUpdate = newNode;
    if (newNode != *toUpdate) {
        printf("Something went wrong on update\nExiting\n");
    }
}

//The GCode instruction Alloc is to allocate empty indirections for use
//in letrec expressions
void alloc(int num, Machine *mach) {
    int i;
    HeapPtr tempInd = NULL;
    for (i = 0; i < num; i++) {
        tempInd = allocIndirection(NULL, globalHeap);
        stackPush(tempInd, &mach->stck);
    }
}
    

int numArgs(Machine *mach) {
    return itemsInFrame(&mach->stck) - 1;
}

void rearrangeStack(int num, stack *stck) {
    HeapPtr *stackElem = NULL;
    HeapPtr *stackElemTo = NULL;
    stackPopThrowAway(stck);
    stackPushEval(NULL, stck);
    int i;
    for (i = 1; i <= num; i++) {
        stackElem = getNthAddrFrom(i, stck, stck->stackPointer);
        stackElemTo = getNthAddrFrom(i-1, stck, stck->stackPointer);
        if ((*stackElem)->tag != APP && (*stackElem)->tag != LOCKED_APP && (*stackElem)->tag != WHNF_APP) {
            printf("Tried to get argument from non-AppNode while rearrangin\nExiting\n");
            exit(1);
        }
        *stackElemTo = (*stackElem)->app.rightArg;
    }
    unlock(*stackElem, WHNF_APP);
}

//TODONOMORE When thread blocks, write out to file with profiling information
ExecutionMode unwind(Machine* mach) {
    HeapPtr item = *mach->stck.stackPointer;
    //this will only be used when unwinding reaches a function call
    int nArgs = -1;
    while (item->tag == APP || item->tag == WHNF_APP || item->tag == INDIRECTION) {
        switch (item->tag) {
            case APP:
                item->tag = LOCKED_APP;
            case WHNF_APP:
                stackPush(item->app.leftArg, &mach->stck);
                item = item->app.leftArg;
                break;
            case INDIRECTION:
                stackPopThrowAway(&mach->stck);
                stackPush(item->indirection, &mach->stck);
                item = item->indirection;
        }
    }

    instruction * newPC = NULL;
    switch (item->tag) {
        case INTEGER:
            //Check to see if evaluation is complete
            //If yes, then do nothing and stop unwinding
            newPC = popFrame(&mach->stck);
            if (newPC == NULL) {
                //end thread
                return FINISHED;
            }
            mach->progCounter = newPC;
            break;
        case CONSTR:
            newPC = popFrame(&mach->stck);
            if (newPC == NULL) {
                //printf("!!!! newPC from unwinding constr returned Null");//end thread
                return FINISHED;
            }
            mach->progCounter = newPC;
            break;
        case FUN:
            nArgs = numArgs(mach);
            if (item->fun.arity == 0) {
                newPC = item->fun.code;
                item->tag = LOCKED_FUN;
                if (newPC == NULL) {
                    printf("Tried to follow code PTR that points to NULL\nExiting\n");
                    exit(1);
                }
                mach->progCounter = newPC + 1;//added 1 to avoid FunDef instruction
                //Need to add sentinal for list of blocked threads
            }
            else if (nArgs < item->fun.arity) {
                HeapPtr *redexRoot = getNthAddrFrom(nArgs, &mach->stck, mach->stck.stackPointer);
                unlock( *redexRoot, WHNF_APP);
                pop(nArgs, mach);
                newPC = popFrame(&mach->stck);
                if (newPC == NULL) {
                    printf("Tried to pop last frame on partial application\nExiting\n");
                    exit(1);
                }
                mach->progCounter = newPC;
            }
            else {
                newPC = item->fun.code;
                rearrangeStack(item->fun.arity, &mach->stck);
                if (newPC == NULL) {
                    printf("Tried to follow code PTR that points to NULL\nExiting\n");
                    exit(1);
                }

                /* When the function being called is `par` we have to make sure
                 * we keep track of which par-site it's from.
                 */
                if (strcmp(newPC->funVals.name, "par") == 0)
                    mach->childrensParSite = item->parSite;

                mach->progCounter = newPC + 1; //added 1 to avoid FunDef instruction
            }
            break;
        case LOCKED_APP:
            addToBlockedQueue(mach, item);
            //printf("Locked Ap case of unwind, set thread to BLOCKED\n");
            /* TODO make profile only */
            mach->blockTime = globalReductions;
            return BLOCKED;
        case LOCKED_FUN:
            printf("Locked function case of unwind, this isn't implemented\n");
            return BLOCKED;
        case COLLECTED:
            printf("WTF THERE ARE COLELCTED SDFLKJSDF\n");
            break;
        default:
            printf("Default case of unwind, this shouldn't happen\n");
            break;
    }
    return LIVE;
}

void eval(Machine *mach) {
    pushFrame(mach->progCounter, &mach->stck);
}

//Below we have all of the two operand arithmetic operators
void addI(Machine *mach) {
    HeapPtr first = stackPopKeep(&mach->stck);
    HeapPtr second = stackPopKeep(&mach->stck);
    int resVal = first->num + second->num;
    HeapPtr res = allocInt(resVal, globalHeap);
    stackPush(res, &mach->stck);
}

void subI(Machine *mach) {
    HeapPtr first = stackPopKeep(&mach->stck);
    HeapPtr second = stackPopKeep(&mach->stck);
    int resVal = first->num - second->num;
    HeapPtr res = allocInt(resVal, globalHeap);
    stackPush(res, &mach->stck);
}

void mulI(Machine *mach) {
    HeapPtr first = stackPopKeep(&mach->stck);
    HeapPtr second = stackPopKeep(&mach->stck);
    int resVal = first->num * second->num;
    HeapPtr res = allocInt(resVal, globalHeap);
    stackPush(res, &mach->stck);
}

void divI(Machine *mach) {
    HeapPtr first = stackPopKeep(&mach->stck);
    HeapPtr second = stackPopKeep(&mach->stck);
    int resVal = first->num / second->num;
    HeapPtr res = allocInt(resVal, globalHeap);
    stackPush(res, &mach->stck);
}


//Below are the unary arithmetic functions (so far only negate)
void negI(Machine *mach) {
    HeapPtr first = stackPopKeep(&mach->stck);
    int resVal = -(first->num);
    HeapPtr res = allocInt(resVal, globalHeap);
    stackPush(res, &mach->stck);
}

//Below are the two operand comparison operators
void eqI(Machine *mach) {
    HeapPtr first = stackPopKeep(&mach->stck);
    HeapPtr second = stackPopKeep(&mach->stck);
    int resVal = first->num - second->num;
    if (resVal == 0)
        resVal = 1;
    else
        resVal = 0;
    HeapPtr res = allocConstr(resVal, 0, globalHeap);
    stackPush(res, &mach->stck);
}
    
void neI(Machine *mach) {
    HeapPtr first = stackPopKeep(&mach->stck);
    HeapPtr second = stackPopKeep(&mach->stck);
    int resVal = first->num - second->num;
    if (resVal != 0)
        resVal = 1;
    else if (resVal == 0)
        resVal = 0;
    HeapPtr res = allocConstr(resVal, 0, globalHeap);
    stackPush(res, &mach->stck);
}
    
void ltI(Machine *mach) {
    HeapPtr first = stackPopKeep(&mach->stck);
    HeapPtr second = stackPopKeep(&mach->stck);
    int resVal = first->num - second->num;
    if (resVal < 0)
        resVal = 1;
    else if (resVal >= 0)
        resVal = 0;
    HeapPtr res = allocConstr(resVal, 0, globalHeap);
    stackPush(res, &mach->stck);
}

void leI(Machine *mach) {
    HeapPtr first = stackPopKeep(&mach->stck);
    HeapPtr second = stackPopKeep(&mach->stck);
    int resVal = first->num - second->num;
    if (resVal <= 0)
        resVal = 1;
    else if (resVal > 0)
        resVal = 0;
    HeapPtr res = allocConstr(resVal, 0, globalHeap);
    stackPush(res, &mach->stck);
}

void gtI(Machine *mach) {
    HeapPtr first = stackPopKeep(&mach->stck);
    HeapPtr second = stackPopKeep(&mach->stck);
    int resVal = first->num - second->num;
    if (resVal > 0)
        resVal = 1;
    else if (resVal <= 0)
        resVal = 0;
    HeapPtr res = allocConstr(resVal, 0, globalHeap);
    stackPush(res, &mach->stck);
}

void geI(Machine *mach) {
    HeapPtr first = stackPopKeep(&mach->stck);
    HeapPtr second = stackPopKeep(&mach->stck);
    int resVal = first->num - second->num;
    if (resVal >= 0)
        resVal = 1;
    else if (resVal < 0)
        resVal = 0;
    HeapPtr res = allocConstr(resVal, 0, globalHeap);
    stackPush(res, &mach->stck);
}
//End of arithmetic and comparison operator


//Casejump deals with checking the constructor tag at the top of the stack, and
//then jumping to the corresponding code sequence. 
void casejump(char *label, Machine *mach) {
    //printf("Casejump here!\n");
    HeapPtr topOfStack = *mach->stck.stackPointer;
    if (topOfStack->tag != CONSTR) {
        printf("Tried to casejump when top of stack was not a constructor\n exiting.\n");
        exit(1);
    }
    int constrTag = topOfStack->constr.id;
    //temporary storage for creatint label to lookup
    char tempIntToStr[10];
    char tempStr[100];
    //create the label
    sprintf(tempIntToStr, ":%d", constrTag);
    strcpy(tempStr, label);
    strcat(tempStr, tempIntToStr);
    //lookup the label
    instruction *newPC = lookupKey(tempStr);
    if (newPC == NULL) {
        printf("Non-exhaustive patterns under label: %s\nExiting...\n", tempStr);
        exit(1);
    }
    mach->progCounter = newPC + 1;
}

void caseAltEnd(char *label, Machine *mach) {
    //temporary storage for creatint label to lookup
    char tempStr[100];
    char endcase[9] = ":EndCase";
    //create the label
    strcpy(tempStr, label);
    strcat(tempStr, endcase);
    //lookup the label
    instruction *newPC = lookupKey(tempStr);
    if (newPC == NULL) {
        printf("Tried to jump out of CaseAltEnd with: %s\nExiting...\n", label);
        exit(1);
    }
    mach->progCounter = newPC + 1;
}

void split(int num, Machine * mach) {
    HeapPtr topOfStack = stackPopKeep(&mach->stck);
    if (topOfStack->tag != CONSTR) {
        printf("Tried to split when Constructor wasn't on top of stack\n exiting.\n");
        exit(1);
    }
    if (topOfStack->constr.arity != num) {
        printf("Arity of constructor does not equal Split value\n exiting.\n");
        exit(1);
    }
    int i = num;
    for (i = i -1; i >=0; i--) {
        stackPush(topOfStack->constr.fields[i].indirection, &mach->stck);
    }
}    

void pack(int tag, int ar, Machine *mach) {
    HeapPtr newConstr = allocConstr(tag, ar, globalHeap);
    int i;
    for (i = 0; i < ar; i++) {
        newConstr->constr.fields[i].indirection = stackPopKeep(&mach->stck);
    }
    stackPush(newConstr, &mach->stck);
}

//This needs to return to dispatchGCode when printing data structures
//shouldn't be too hard but requires that the PC keeps going back to Eval and Print
void printI(Machine *mach) {
    evalPrintLoop -= 1;
    HeapPtr oldTop = stackPopKeep(&mach->stck);
    if (oldTop->tag == INTEGER) {
        printf("%d ", oldTop->num);
    }
    else if (oldTop->tag == CONSTR) {
        int i = oldTop->constr.arity;
        evalPrintLoop += i;
        printf("<%d> ", oldTop->constr.id);
        for (i = i -1; i >=0; i--) {
            stackPush(oldTop->constr.fields[i].indirection, &mach->stck);
        }
    }
    else {
        printf("Trying to print non-Int or non-Constructor!\n");
    }

    if (evalPrintLoop != 0) {
        //eval(mach);
        mach->progCounter -= 2;
    }
   // printf("\nTotal Reductions: %d\n", globalReductions);
    //exit(0);
}

//TODO initialize machine with parID and a new threadID
void parI(Machine* mach, threadPool* pool) {
    if (mach->creationOH > 0) {
        // reset code pointer to execute PAR again
        mach->progCounter -= 1;
        mach->creationOH -= 1;
        return;
    } else if (mach->creationOH == 0) {
        mach->creationOH = pool->createOH;
    }

    //get heap address that the new thread will start computing from
    HeapPtr topOfStack = stackPopKeep(&mach->stck);

    //allocate and initialize a new Machine
    Machine* tempMachPtr = malloc(sizeof(Machine));
    initMachine(tempMachPtr, pool->initOH, pool->createOH);
    stackPush(topOfStack, &tempMachPtr->stck);

    /* set the new machine's parSite and creatorID */
    tempMachPtr->parSite = mach->childrensParSite;
    tempMachPtr->creatorID = mach->threadID;

    //Add machine to thread pool
    addMachToThreadPool(tempMachPtr, pool);
}
