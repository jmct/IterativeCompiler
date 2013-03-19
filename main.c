#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "instructions.h"
#include "heap.c"
#include "lex.yy.c"

#define HEAPSIZE 10000
#define STACK_SIZE 3000
#define FRAME_STACK_SIZE 1000



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

void initMachine(Machine *mach) {
    mach->progCounter  = 0;
    mach->stackPointer = 0;
    mach->framePointer = 0;
}

//This pops one item off the stack for when we want to STORE the item
//this function DOES NOT correspond to the GCode instruction 'pop'
Heap popStack(Machine *mach) {
    HeapCell *poppedItem;
    poppedItem = mach->stack[mach->stackPointer];
    mach->stackPointer--;
    return poppedItem;
}

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


//Any top-level function will be pushed onto to the stack
//via this function
void pushGlobal(instruction fun, Machine *mach) {
    int codePtr = lookupKey(fun.funVals.name);    
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

instruction makeInstruction(char *instr) {
    instruction newInstr;
    tokenTag newRes;
    if (strcmp(instr, "Unwind") == 0) {
        newInstr.type = Unwind;
        return newInstr;
    }
    else if (strcmp(instr, "Eval") == 0) {
        newInstr.type = Eval;
        return newInstr;
    }
    else if (strcmp(instr, "MkAp") == 0) {
        newInstr.type = MkAp;
        return newInstr;
    }
    else if (strcmp(instr, "Print") == 0) {
        newInstr.type = Print;
        return newInstr;
    }
    else if (strcmp(instr, "Par") == 0) {
        newInstr.type = Par;
        return newInstr;
    }
    else if (strcmp(instr, "Add") == 0) {
        newInstr.type = Add;
        return newInstr;
    }
    else if (strcmp(instr, "Sub") == 0) {
        newInstr.type = Sub;
        return newInstr;
    }
    else if (strcmp(instr, "Mul") == 0) {
        newInstr.type = Mul;
        return newInstr;
    }
    else if (strcmp(instr, "Div") == 0) {
        newInstr.type = Div;
        return newInstr;
    }
    else if (strcmp(instr, "Neg") == 0) {
        newInstr.type = Neg;
        return newInstr;
    }
    else if (strcmp(instr, "Eq") == 0) {
        newInstr.type = Eq;
        return newInstr;
    }
    else if (strcmp(instr, "Lt") == 0) {
        newInstr.type = Lt;
        return newInstr;
    }
    else if (strcmp(instr, "Le") == 0) {
        newInstr.type = Le;
        return newInstr;
    }
    else if (strcmp(instr, "Gt") == 0) {
        newInstr.type = Gt;
        return newInstr;
    }
    else if (strcmp(instr, "Ge") == 0) {
        newInstr.type = Ge;
        return newInstr;
    }
    else if (strcmp(instr, "Ne") == 0) {
        newInstr.type = Ne;
        return newInstr;
    }
    else if (strcmp(instr, "FunDef:") == 0) {
        newInstr.type = FunDef;
        newRes = yylex();
        if (newRes == Argument) {
            printf("GCode badly formatted at: FunDef %d\nExiting.\n", yyval.intVal);
            exit(1);
        }
        newInstr.funVals.name = malloc(strlen(yyval.strVal) + 1);
        strcpy(newInstr.funVals.name, yyval.strVal);
        newRes = yylex();
        if (newRes != Argument) {
            printf("GCode badly formatted at: Pack (tag) %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        newInstr.funVals.arity = yyval.intVal;
        return newInstr;
    }
    else if (strcmp(instr, "PushGlobal") == 0) {
        newInstr.type = PushGlobal;
        newRes = yylex();
        if (newRes == Argument) {
            printf("GCode badly formatted at: PushGlobal %d\nExiting.\n", yyval.intVal);
            exit(1);
        }
        else if (newRes == Instruction) {
            printf("GCode badly formatted at: PushGlobal %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        newInstr.pushGlobVal = malloc(strlen(yyval.strVal) + 1);
        strcpy(newInstr.pushGlobVal, yyval.strVal);
        return newInstr;
    }
    else if (strcmp(instr, "PushInt") == 0) {
        newInstr.type = PushInt;
        newRes = yylex();
        if (newRes != Argument) {
            printf("GCode badly formatted at: PushInt %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        newInstr.pushIntVal = yyval.intVal;
        return newInstr;
    }
    else if (strcmp(instr, "Push") == 0) {
        newInstr.type = Push;
        newRes = yylex();
        if (newRes != Argument) {
            printf("GCode badly formatted at: Push %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        newInstr.pushVal = yyval.intVal;
        return newInstr;
    }
    else if (strcmp(instr, "Update") == 0) {
        newInstr.type = Update;
        newRes = yylex();
        if (newRes != Argument) {
            printf("GCode badly formatted at: Update %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        newInstr.updateVal = yyval.intVal;
        return newInstr;
    }
    else if (strcmp(instr, "Pop") == 0) {
        newInstr.type = Pop;
        newRes = yylex();
        if (newRes != Argument) {
            printf("GCode badly formatted at: Pop %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        newInstr.popVal = yyval.intVal;
        return newInstr;
    }
    else if (strcmp(instr, "Slide") == 0) {
        newInstr.type = Slide;
        newRes = yylex();
        if (newRes != Argument) {
            printf("GCode badly formatted at: Slide %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        newInstr.slideVal = yyval.intVal;
        return newInstr;
    }
    else if (strcmp(instr, "Alloc") == 0) {
        newInstr.type = Alloc;
        newRes = yylex();
        if (newRes != Argument) {
            printf("GCode badly formatted at: Alloc %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        newInstr.allocVal = yyval.intVal;
        return newInstr;
    }
    else if (strcmp(instr, "Pack") == 0) {
        newInstr.type = Pack;
        newRes = yylex();
        if (newRes != Argument) {
            printf("GCode badly formatted at: Pack %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        newInstr.packVals.tag = yyval.intVal;
        newRes = yylex();
        if (newRes != Argument) {
            printf("GCode badly formatted at: Pack (tag) %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        newInstr.packVals.arity = yyval.intVal;
        return newInstr;
    }
    else if (strcmp(instr, "CaseJump:") == 0) {
        newInstr.type = CaseJump;
        newRes = yylex();
        if (newRes == Instruction) {
            printf("GCode badly formatted at: CaseJump: %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        else if (newRes == Argument) {
            printf("GCode badly formatted at: CaseJump: %d\nExiting.\n", yyval.intVal);
            exit(1);
        }
        newInstr.caseJumpVal = malloc(strlen(yyval.strVal) + 1);
        strcpy(newInstr.caseJumpVal, yyval.strVal);
        return newInstr;
    }
    else if (strcmp(instr, "CaseAlt:") == 0) {
        newInstr.type = CaseAlt;
        newRes = yylex();
        char tempStr[10];
        char tempIntToStr[10];
        if (newRes == Instruction) {
            printf("GCode badly formatted at: CaseAlt: %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        else if (newRes == Argument) {
            printf("GCode badly formatted at: CaseAlt: %d\nExiting.\n", yyval.intVal);
            exit(1);
        }
        strcpy(tempStr, yyval.strVal);
        newRes = yylex();
        if (newRes != Argument) {
            printf("GCode badly formatted at: CaseAlt: (something) %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        sprintf(tempIntToStr, "%d", yyval.intVal);
        newInstr.caseAltVal = malloc(strlen(tempStr) + strlen(tempIntToStr) + 1);
        strcpy(newInstr.caseAltVal, tempStr);
        strcat(newInstr.caseAltVal, tempIntToStr);
        //old below
        return newInstr;
    }
    else if (strcmp(instr, "CaseAltEnd:") == 0) {
        newInstr.type = CaseAltEnd;
        newRes = yylex();
        if (newRes == Instruction) {
            printf("GCode badly formatted at: CaseAltEnd: %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        else if (newRes == Argument) {
            printf("GCode badly formatted at: CaseAltEnd: %d\nExiting.\n", yyval.intVal);
            exit(1);
        }
        newInstr.caseAltEndVal = malloc(strlen(yyval.strVal) + 1);
        strcpy(newInstr.caseAltEndVal, yyval.strVal);
        return newInstr;
    }
    else if (strcmp(instr, "Split") == 0) {
        newInstr.type = Split;
        newRes = yylex();
        if (newRes != Argument) {
            printf("GCode badly formatted at: Split %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        newInstr.splitVal = yyval.intVal;
        return newInstr;
    }
    else if (strcmp(instr, "Label:") == 0) {
        newInstr.type = GLabel;
        newRes = yylex();
        if (newRes == Instruction) {
            printf("GCode badly formatted at: CaseAltEnd: %s\nExiting.\n", yyval.strVal);
            exit(1);
        }
        else if (newRes == Argument) {
            printf("GCode badly formatted at: CaseAltEnd: %d\nExiting.\n", yyval.intVal);
            exit(1);
        }
        newInstr.labelVal = malloc(strlen(yyval.strVal) + 1);
        strcpy(newInstr.labelVal, yyval.strVal);
        return newInstr;
    }
    //NOTHING BELOW THIS LINE (in this function) IS FINISHED!!!!!
}

instruction *parseGCode() {
    instruction * prog = malloc(sizeof(instruction)* 100);
    instruction * temp = NULL; //This is to hold a temp pointer when we realloc()
    int currentSize = 100;
    int curInstr = 0;
    tokenTag res;
    res = yylex();
    while (res != END) {
        if (res == Instruction) {
            printf("Instruction(%s)", yyval.strVal);
            prog[curInstr] = makeInstruction(yyval.strVal);
        }
        else {
            printf("There is an error in the formatting of the GCode\n");
        }
        curInstr += 1;
        //Check to make sure that we still have space in our Program array.
        if (curInstr >= currentSize) {
            temp = realloc(prog, sizeof(instruction) * currentSize * 2);
            if (temp != NULL) {
                currentSize *= 2;
                prog = temp;
            }
            else {
                printf("Error in allocating memory when loading GCode into memory. Exiting\n");
                exit(1);
            }
        }
        res = yylex();
    } 
    if (curInstr < currentSize) {
        temp = realloc(prog, sizeof(instruction) * curInstr);
        if (temp != NULL) {
            prog = temp;
        }
        else {
            printf("Error in allocating memory when loading GCode into memory. Exiting\n");
            exit(1);
        }
    }
    return prog;
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
    prog = parseGCode();

    printf("Size of program: %d instructions.\n", (sizeof(prog)/sizeof(prog[0])));

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
