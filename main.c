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
        char* name;
        int pushIntVal;
        int pushVal;
        int updateVal;
        int popVal;
        int slideVal;
        int allocVal;
        int splitVal;
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

//Pushing an arbitrary item onto the stack
void pushStack(HeapCell* item, Machine* mach) {
    mach->stack[mach->stackPointer + 1] = item;
    mach->stackPointer++;
}

void pushGlobal(char *name, Machine *mach) {
    int codePtr = lookupKey(name);    
    Heap addr = allocFun(arity, codePtr);
    pushStack(addr, mach);
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

/*
void unwind(HeapCell* item, Machine* machine) {
    do {
        switch (item->tag) {
            case APP:
                pushStack(item->app.leftArg, machine);
                item = item->app.leftArg;
                break;
            case FUN:
                
}
*/

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
