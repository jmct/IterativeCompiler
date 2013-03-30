#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "instructions.h"
#include "symbolTable.h"
#include "stack.h"
#include "heap.c"
//#include "lex.yy.c"

#define HEAPSIZE 10000
#define STACK_SIZE 3000
#define FRAME_STACK_SIZE 1000



typedef struct {
    stack stck;
    instruction* progCounter;
} Machine;

void initMachine(Machine *mach) {
    mach->progCounter  = NULL;
    mach->stack = initStack(mach->stck);
}

//This is for the GCode instruction 'Slide n'
void slideNStack(int n, Machine *mach) {
    HeapCell *temp = popStack(mach);
    popNFromStack(n, mach);
    pushStack(temp, mach);
}


//Any top-level function will be pushed onto to the stack
//via this function
void pushGlobal(instruction fun, Machine *mach) {
    instruction* codePtr = lookupKey(fun.funVals.name);    
    Heap addr = allocFun(fun.funVals.arity, codePtr);
    pushStack(addr, mach);
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
    leftArg = popStack(mach);
    rightArg = popStack(mach);
    newNode = allocApp(leftArg, rightArg);
    pushStack(newNode, mach);
}

//After an expression is evaluated, the root node of the 
//expression (which is n+1 items into the stack
//must be updated in order to allow for sharing
void update(int n, Machine *mach) {
    HeapCell *indirectTo = popStack(mach);
    HeapCell *indirectNode = allocIndirection(indirectTo);
    mach->stack[mach->stackPointer - n] = indirectNode;
}

//alloc is used in letrec expressions to ensure that 
//there is heap allocated for (as of yet) unknown expressions
void allocN(int n, Machine *mach) {
    HeapCell *tempAddr; 
    for (n; n > 0; n--) {
        tempAddr = allocIndirection(NULL);
        pushStack(tempAddr, mach);
    }
}

/*
void evalI(Machine *mach) {
    
}
*/

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
    /* Old test code, will be used again
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
    printf("Now applying mkAp:\n");
    mkAp(&machineA);
    showMachineState(&machineA);
    */
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
