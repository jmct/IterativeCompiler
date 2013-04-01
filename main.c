#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "instructions.h"
#include "symbolTable.h"
#include "heap.h"
#include "stack.h"
//#include "lex.yy.c"

#define HEAPSIZE 10000
#define STACK_SIZE 3000
#define FRAME_STACK_SIZE 1000


HeapPtr globalHeap = NULL;

typedef struct {
    stack stck;
    instruction* progCounter;
} Machine;

void initMachine(Machine *mach) {
    mach->progCounter  = NULL;
    mach->stck = initStack(mach->stck);
}

//This is for the GCode instruction 'Slide n'
void slideNStack(int n, Machine *mach) {
    HeapCell *temp = stackPopKeep(&mach->stck);
    popNFromStack(n, &mach->stck);
    stackPush(temp, &mach->stck);
}


//Any top-level function will be pushed onto to the stack
//via this function
void pushGlobal(instruction fun, Machine *mach) {
    instruction* codePtr = lookupKey(fun.funVals.name);    
    HeapPtr addr = allocFun(fun.funVals.arity, codePtr, globalHeap);
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
        
//update the pointer to the top of the expression tree to point
//to an indirection node (this allows for sharing)
//TODO:
//When locked nodes introduced, this function must take them into accont
void update(int num, Machine *mach) {
    HeapCell **toUpdate = NULL;
    HeapCell * top = stackPopKeep(&mach->stck);
    HeapCell *newNode = allocIndirection(top, globalHeap);
    toUpdate = getNthAddrFrom(num, &mach->stck, mach->stck.stackPointer);
    *toUpdate = newNode;
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
*/

//alloc is used in letrec expressions to ensure that 
//there is heap allocated for (as of yet) unknown expressions
void allocN(int n, Machine *mach) {
    HeapCell *tempAddr; 
    for (n; n > 0; n--) {
        tempAddr = allocIndirection(NULL, globalHeap);
        stackPush(tempAddr, &mach->stck);
    }
}

/*
void evalI(Machine *mach) {
    
}
*/

int numArgs(Machine *mach) {
    return itemsInFrame(&mach->stck) - 1;
}

void unwind(HeapCell* item, Machine* machine) {
    //this will only be used when unwinding reaches a function call
    int nArgs = -1;

    while (item->tag == APP) {
        stackPush(item->app.leftArg, &machine->stck);
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
    printf("About to enter parseGCode()\n");
    prog = parseGCode();
    int counter;
    instruction * tempInstrPtr = NULL;
    for (counter = 0; prog[counter].type != End; counter++) {
        if (prog[counter].type == CaseAlt || prog[counter].type == GLabel) {
            tempInstrPtr = lookupKey(prog[counter].labelVal);
            printf("ArrayIndex ptr Value: %d\nTable lookup value: %d\n\n", counter, 
                    (int)(tempInstrPtr - &prog[0]));
        }
        else if (prog[counter].type == FunDef) {
            tempInstrPtr = lookupKey(prog[counter].funVals.name);
            printf("FunDef position: %d\nLookup val: %d\n\n", counter,
                    (int)(tempInstrPtr - &prog[0]));
        }
        printf("%d\n", prog[counter].type);
    }
    printf("\nCounter value = %d", counter);
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
