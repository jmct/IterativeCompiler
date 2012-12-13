#include <stdio.h>
#include <stdlib.h>
#include "heap.c"

#define HEAPSIZE 10000
#define STACK_SIZE 3000
#define FRAME_STACK_SIZE 1000

typedef enum {
         Unwind, 
         PushGlobal,
         PushInt,
         Push,
         MkAp,
         Update,
         Pop,
         Slide,
         Alloc,
         Eval,
         Add, Sub, Mul, Div, Neg,
         Eq, Ne, Lt, Le, Gt, Ge,
         Cond,
         Pack,
         Casejump,
         Split, 
         Print,
         Par,
} GCode;

typedef int codePtr;

struct instruction {
    GCode type;
    union {
        int pushIntVal;
        int pushVal;
        int updateVal;
        int popVal;
        int slideVal;
        int allocVal;
        int splitVal;
        struct {
            int arity;
            char *name;
        } funVals;
        struct {
            codePtr tr, fa;
        } condVals;
        struct {
            int tag, arity;
        } packVals;
    };

};

typedef struct {
    int currentFP;
    int retProgCounter;
} Frame;


typedef struct {
    HeapCell * stack[STACK_SIZE];
    int progCounter;
    int stackPointer; 
    int framePointer;
    Frame frameStack[FRAME_STACK_SIZE];
} Machine;

int numArgs(Machine *mach) {
    return mach->stackPointer - mach->framePointer;
}

//Pushing an arbitrary item onto the stack
void pushStack(HeapCell* item, Machine* mach) {
    mach->stack[mach->stackPointer + 1] = item;
    mach->stackPointer++;
}

void pushNStack(int n, Machine *mach) {
    HeapCell *toPush = mach->stack[mach->stackPointer - n];
    pushStack(toPush, mach);
}

//This is the function that corresponds to the GCode instruction 'Pop n'
void popNFromStack(int n, Machine *mach) {
    mach->stackPointer -= n;
}

//This is for the GCode instruction 'Slide n'
void slideNStack(int n, Machine *mach) {
    HeapCell *temp = popStack(mach);
    popNFromStack(n, mach);
    pushStack(temp, mach);
}

//This pops one item off the stack for when we want to STORE the item
//this function DOES NOT correspond to the GCode instruction 'pop'
Heap popStack(Machine *mach) {
    HeapCell *poppedItem;
    poppedItem = mach->stack[mach->stackPointer];
    mach->stackPointer--;
    return poppedItem;
}

void pushGlobal(struct instruction fun, Machine *mach) {
    int codePtr = lookupKey(fun.funVals.name);    
    Heap addr = allocFun(fun.funVals.arity, codePtr);
    pushStack(addr, mach);
}

void pushInt(int val, Machine * mach) {
    //TODO 
}

void mkAp(Machine *mach) {
    HeapCell *leftArg, *rightArg, *newNode;
    leftArg = popStack(mach);
    rightArg = popStack(mach);
    newNode = allocApp(leftArg, rightArg);
    pushStack(newNode, mach);
}

void update(int n, Machine *mach) {
    HeapCell *indirectTo = popStack(mach);
    //See about unlocking. 
}

void initMachine(Machine *mach) {
    mach->progCounter  = 0;
    mach->stackPointer = 0;
    mach->framePointer = 0;
}

//When Eval is executed we must update the stack frame to 
//represent the environment of the new function being 
//unwound
void pushFrame(Machine *mach) {
    Frame *newFrame = &(mach->frameStack[mach->framePointer +1]);
    newFrame->retProgCounter = mach->progCounter + 1; //I am not sure about this
    newFrame->currentFP = mach->framePointer; 
    mach->framePointer = mach->stackPointer;
}

void unwind(HeapCell* item, Machine* machine) {
    //this will only be used when unwinding reaches a function call
    int nArgs = -1;

    while (item->tag == APP) {
        pushStack(item->app.leftArg, machine);
        item = item->app.leftArg;
    }

    switch (item->tag) {
        case INTEGER:
            //Check to see if evaluation is complete
            //If yes, then do nothing and stop unwinding
            break;
        case FUN:
            nArgs = numArgs(machine);
            if (nArgs < item->fun.arity) {
                //pop nArgs items off of stack
                //pop  stack frame, ensuring that 
                //The top of the stack (from two lines up) stays the same
            } else {
                //perform 'rearrange' 
                machine->progCounter = item->fun.code;
            }
        default:
            break;
    }
}

void showMachineState(Machine *mach) {
    printf("Machine Stack:\n");
    int stackDepth = mach->stackPointer;
    for (stackDepth; stackDepth > 0; stackDepth--) {
        printf("\t");
        showHeapItem(*mach->stack[stackDepth]);
    }
}


int main() {
    Machine machineA;
    printf("machineA's stack pointer is at: %d\n", machineA.stackPointer);
    myHeap = malloc(HEAPSIZE * sizeof(HeapCell));
    printf("Free: %d, Pointer Value %p\n", nextFree, myHeap);
    Heap point = allocHeapCell(APP, myHeap); 
    printf("Free: %d, Pointer Value %p\n", nextFree, point);
    pushStack(point, &machineA);
    printf("machineA's stack pointer is at: %d\n", machineA.stackPointer);
    point = allocHeapCell(APP, myHeap); 
    printf("Free: %d, Pointer Value %p\n", nextFree, point);
    pushStack(point, &machineA);
    showMachineState(&machineA);

    return 0;
}
