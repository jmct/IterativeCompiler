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

/*
void pushInt(int val, Machine * mach) {
    //TODO 
}
*/

//MkAp simply takes the two topmost items on the stack
//and replaces them with an application node pointing 
//to both
void mkAp(Machine *mach) {
    HeapCell *leftArg, *rightArg, *newNode;
    leftArg = stackPopKeep(mach);
    rightArg = stackPopKeep(mach);
    newNode = allocApp(leftArg, rightArg, globalHeap);
    stackPush(newNode, &mach->stck);
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
    printf("machineA's stack pointer is at: %d\n", machineA.stackPointer);
    myHeap = malloc(HEAPSIZE * sizeof(HeapCell));
    printf("Free: %d, Pointer Value %p\n", nextFree, myHeap);
    Heap point = allocHeapCell(APP, myHeap); 
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
    int tempLookup = 0;
    for (counter = 0; prog[counter].type != End; counter++) {
        if (prog[counter].type == CaseAlt || prog[counter].type == GLabel) {
            tempLookup = lookupKey(prog[counter].labelVal);
            printf("ArrayIndex ptr Value: %d\nTable lookup value: %d\n\n", counter, tempLookup);
        }
        else if (prog[counter].type == FunDef) {
            tempLookup = lookupKey(prog[counter].funVals.name);
            printf("FunDef position: %d\nLookup val: %d\n\n", counter, tempLookup);
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
