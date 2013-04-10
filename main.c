#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "instructions.h"
#include "symbolTable.h"
#include "heap.h"
#include "stack.h"
#include "gthread.h"
//#include "lex.yy.c"
#define NUM_CORES 2
#define HEAPSIZE 1000000
#define STACK_SIZE 2000
#define FRAME_STACK_SIZE 1000

struct Machine_ {
    stack stck;
    instruction* progCounter;
};

typedef struct Machine_ Machine;

void initMachine(Machine *mach) {
    mach->progCounter  = NULL;
    mach->stck = initStack(mach->stck);
}

enum ExecutionMode_ {
    LIVE,   
    BLOCKED,
    FINISHED
};

typedef enum ExecutionMode_ ExecutionMode;

HeapPtr globalHeap = NULL;
threadPool* globalPool = NULL;


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
    //the reason we do not add 1 to codePtr above is so that we can get the
    //arity information from the function definition instruction.
    //this type of information could be added to the symbol table to more
    //efficiency. (So the ST wouldn't just store addresses but structs that
    //had all relevant information based on the tag of the result of the lookup    
    HeapPtr addr = allocFun(codePtr->funVals.arity, codePtr, globalHeap);
    //don't do effectful expressions in parameter list! adding 1 to codePtr can break 
    //lookup for first parameter. 
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
    newSP = getNthAddrFrom(num, &mach->stck, mach->stck.stackPointer);
    mach->stck.stackPointer = newSP;
    stackPush(top, &mach->stck);
}

//Pop the top N elements off the stack
void pop(int num, Machine *mach) {
    HeapCell **newSP = NULL;
    newSP = getNthAddrFrom(num, &mach->stck, mach->stck.stackPointer);
    mach->stck.stackPointer = newSP;
}

void unlock(HeapPtr node) {
    while (node->tag == LOCKED_APP) {
        //TODO empty pending list <--I think it's done
        if (node->app.numBlockedThreads > 0) {
            addQueueToThreadPool(node->app.blockedQueue, node->app.numBlockedThreads, globalPool);
            node->app.numBlockedThreads = 0;
            node->app.blockedQueue = NULL;
        }
        node->tag = APP;
        node = node->app.leftArg;
    }
    if (node->tag == LOCKED_FUN) {
        if (node->fun.numBlockedThreads > 0) {
            addQueueToThreadPool(node->fun.blockedQueue, node->fun.numBlockedThreads, globalPool);
            node->fun.numBlockedThreads = 0;
            node->fun.blockedQueue = NULL;
        }
        //TODO empty pending list <--I think it's done
        node->tag = FUN;
    }
    else {
        return;
    }
}
        
        
//update the pointer to the top of the expression tree to point
//to an indirection node (this allows for sharing)
//TODO:
//When locked nodes introduced, this function must take them into accont
void update(int num, Machine *mach) {
    HeapCell **toUpdate = NULL;
    HeapCell * top = stackPopKeep(&mach->stck);
    toUpdate = getNthAddrFrom(num, &mach->stck, mach->stck.stackPointer);
    if (*toUpdate == NULL) {
        printf("Items in frame: %d, update parameter val: %d\n", itemsInFrame(&mach->stck), num);
    }
    unlock(*toUpdate);
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
    
/*
//After an expression is evaluated, the root node of the 
//expression (which is n+1 items into the stack
//must be updated in order to allow for sharing
void update(int n, Machine *mach) {
    HeapCell *indirectTo = popStack(mach);
    HeapCell *indirectNode = allocIndirection(indirectTo);
    mach->stck[(int)mach->stck.stackPointer - n] = indirectNode;
}

//alloc is used in letrec expressions to ensure that 
//there is heap allocated for (as of yet) unknown expressions
void allocN(int n, Machine *mach) {
    HeapCell *tempAddr; 
    for (n; n > 0; n--) {
        tempAddr = allocIndirection(NULL, globalHeap);
        stackPush(tempAddr, &mach->stck);
    }
}
*/

int numArgs(Machine *mach) {
    return itemsInFrame(&mach->stck) - 1;
}

void rearrangeStack(int num, stack *stck) {
    HeapPtr *stackElem = NULL;
    HeapPtr *stackElemTo = NULL;
    stackPopThrowAway(stck);
    stackPush(NULL, stck);
    int i;
    for (i = 1; i <= num; i++) {
        stackElem = getNthAddrFrom(i, stck, stck->stackPointer);
        stackElemTo = getNthAddrFrom(i-1, stck, stck->stackPointer);
        if ((*stackElem)->tag != APP && (*stackElem)->tag != LOCKED_APP) {
            printf("Tried to get argument from non-AppNode while rearrangin\nExiting\n");
            exit(1);
        }
        *stackElemTo = (*stackElem)->app.rightArg;
    }
}

ExecutionMode unwind(Machine* mach) {
    HeapPtr item = *mach->stck.stackPointer;
    //this will only be used when unwinding reaches a function call
    int nArgs = -1;
    int i;
    while (item->tag == APP || item->tag == INDIRECTION) {
        if (item->tag == APP) {
            item->tag = LOCKED_APP;
            stackPush(item->app.leftArg, &mach->stck);
            item = item->app.leftArg;
        }
        if (item->tag == INDIRECTION) {
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
                printf("!!!! newPC from unwinding constr returned Null");//end thread
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
                mach->progCounter = newPC + 1; //added 1 to avoid FunDef instruction
            }
            break;
        case LOCKED_APP:
            addToBlockedQueue(mach, item);
            printf("Locked Ap case of unwind, set thread to BLOCKED\n");
            return BLOCKED;
        case LOCKED_FUN:
            printf("Locked function case of unwind, this isn't implemented\n");
            return BLOCKED;
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
        stackPush(topOfStack->constr.fields[i], &mach->stck);
    }
}    

void pack(int tag, int ar, Machine *mach) {
    HeapPtr newConstr = allocConstr(tag, ar, globalHeap);
    int i;
    for (i = 0; i < ar; i++) {
        newConstr->constr.fields[i] = stackPopKeep(&mach->stck);
    }
    stackPush(newConstr, &mach->stck);
}

void printI(Machine *mach) {
    HeapPtr oldTop = stackPopKeep(&mach->stck);
    if (oldTop->tag == INTEGER) {
        printf("%d ", oldTop->num);
    }
    else if (oldTop->tag == CONSTR) {
        int i = oldTop->constr.arity;
        printf("<%d> ", oldTop->constr.id);
        for (i = i -1; i >=0; i--) {
            stackPush(oldTop->constr.fields[i], &mach->stck);
        }
        eval(mach);
        mach->progCounter -= 1;
    }
    else {
        printf("Trying to print non-Int or non-Constructor!\n");
    }
    printf("\n");
    exit(0);
}

void parI(Machine* mach, threadPool* pool) {
    //get heap address that the new thread will start computing from
    HeapPtr topOfStack = stackPopKeep(&mach->stck);
    //allocate and initialize a new Machine
    Machine* tempMachPtr = malloc(sizeof(Machine));
    initMachine(tempMachPtr);
    stackPush(topOfStack, &tempMachPtr->stck);
    //Add machine to thread pool
    addMachToThreadPool(tempMachPtr, pool);
}


/*
void showMachineState(Machine *mach) {
    printf("Machine Stack:\n");
    int stackDepth = mach->stackPointer;
    for (stackDepth; stackDepth > 0; stackDepth--) {
        printf("\t");
        showHeapItem(*mach->stack[stackDepth]);
    }
}
*/

ExecutionMode dispatchGCode(Machine *mach);

int main() {
    /* Old test code, will be used again
    Machine machineA;
    initMachine(&machineA);
    printf("machineA's stack pointer is at: %d\n", machineA.stackPointer);
    myHeap = malloc(HEAPSIZE * sizeof(HeapCell));
    printf("Free: %d, Pointer Value %p\n", nextFree, myHeap);
    HeapPtr point = allocHeapCell(APP, myHeap); 
    printf("Free: %d, Pointer Value %p\n", nextFree, point);
    stackPush(point, &machineA.stck);
    printf("machineA's stack pointer is at: %d\n", machineA.stackPointer);
    point = allocHeapCell(APP, myHeap); 
    printf("Free: %d, Pointer Value %p\n", nextFree, point);
    stackPush(point, &machineA.stck);
    showMachineState(&machineA);
    printf("Now applying mkAp:\n");
    mkAp(&machineA);
    showMachineState(&machineA);
    */
    globalHeap = malloc(sizeof(HeapCell) * HEAPSIZE);
    instruction *prog = NULL;
    //printf("About to enter parseGCode()\n");
    prog = parseGCode();
    int counter;
    instruction * tempInstrPtr = NULL;
    for (counter = 4; prog[counter].type != End; counter++) {
        if (prog[counter].type == CaseAlt || prog[counter].type == GLabel) {
            tempInstrPtr = lookupKey(prog[counter].labelVal);
     //       printf("ArrayIndex ptr Value: %d\nTable lookup value: %d\n", counter, 
         //           (int)(tempInstrPtr - &prog[0]));
      //      printf("Label Value: %s\n\n", prog[counter].labelVal);
        }
        else if (prog[counter].type == FunDef) {
            tempInstrPtr = lookupKey(prog[counter].funVals.name);
       //     printf("FunDef position: %d\nLookup val: %d\n\n", counter,
        //            (int)(tempInstrPtr - &prog[0]));
        }
    //    printf("%d\n", prog[counter].type);
    }
   // printf("\nCounter value = %d\n", counter);
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
    cores[0] = malloc(sizeof(Machine));
    initMachine(cores[0]);
    cores[0]->progCounter = prog;

    //TODO when core is no longer running, we need to free the machine and it's
    //stack... etc
    while (programMode == LIVE) {
        programMode = FINISHED;
        for (i = 0; i < NUM_CORES; i++) {
            Machine* fromThreadPool = NULL;
            //see if the core needs to pull from the spark pool
            if (cores[i] == NULL) {
                fromThreadPool = getMachFromPool(globalPool);
                if (fromThreadPool != NULL) {
                    cores[i] = fromThreadPool;
                    programMode = LIVE;
                    //if the thread is a new spark, the PC will be NULL
                    if (cores[i]->progCounter == NULL) {
                        eval(cores[i]);
                        core = unwind(cores[i]);
                    }
                    else {
                        core = unwind(cores[i]);
                    }
                }
            }
            else { 
                programMode = LIVE; 
                core = dispatchGCode(cores[i]); 
            }

            if (core != LIVE) {
                cores[i] = NULL;
            }
        }
    }
    return 0;
}
/*
main() {
    tokenTag res;
    res = yylex();
    while (res != END) {
        if (res == Instruction)
            printf("Instruction(%s)", yyval.strVal);
        else if (res == Label)
            printf("Label(%s)", yyval.strVal);
        else if (res == Argument)
            printf("Arg(%d)", yyval.intVal);
        res = yylex();
    } 
}
typedef enum {
    Instruction,
    Label,
    Argument,
    END
} tokenTag;

union {
    int intVal;
    char* strVal;
} yyval;
*/

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
            printf("WE HAVE REACHED A FUNDEF AND FINISHED THREAD");
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
