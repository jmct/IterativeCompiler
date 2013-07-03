#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lex.yy.c"
#include "instructions.h"
#include "symbolTable.h"
//The following are the definitions for the functions that parse our gcode
//file. 

int parTagCount;

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
            printf("GCode badly formatted at: FunDef (label) %s\nExiting.\n", yyval.strVal);
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
        if (strcmp(newInstr.pushGlobVal, "par") == 0) {
            newInstr.parTag = parTagCount; //The reason we have to tag it hear is 
                                           //because we need the unique par positions 
                                           //in the GCode and this is the only chance. 
            parTagCount++;
        }
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
        newInstr.labelVal = malloc(strlen(yyval.strVal) + 1);
        strcpy(newInstr.labelVal, yyval.strVal);
        return newInstr;
    }
    else if (strcmp(instr, "CaseAlt:") == 0) {
        newInstr.type = CaseAlt;
        newRes = yylex();
        char tempStr[100];
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
        newInstr.labelVal = malloc(strlen(tempStr) + strlen(tempIntToStr) + 1);
        strcpy(newInstr.labelVal, tempStr);
        strcat(newInstr.labelVal, tempIntToStr);
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
        newInstr.labelVal = malloc(strlen(yyval.strVal) + 1);
        strcpy(newInstr.labelVal, yyval.strVal);
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
    printf("Encountered illegal GCode instruction: %s\nExiting...\n", instr);
    exit(1);
    //NOTHING BELOW THIS LINE (in this function) IS FINISHED!!!!!
}

void setupIntro(instruction *prog) {
    //Below are the three instructions that start everything off
    //PushGlobal "main"
    //Eval
    //Print
    //End
    instruction intro1, intro2, intro3, intro4;
    intro1.type = PushGlobal;
    intro1.pushGlobVal = malloc(sizeof(char) * 5); //"main" + 1
    char mn[] = "main";
    strcpy(intro1.pushGlobVal, mn);
    prog[0] = intro1;
    intro2.type = Eval;
    prog[1] = intro2;
    intro3.type = Print;
    prog[2] = intro3;
    intro4.type = End;
    prog[3] = intro4;
}

instruction *parseGCode(FILE* gcodeFile) {
    //printf("Entered parseGCode()\n");
    instruction * prog = malloc(sizeof(instruction)* 100);
    setupIntro(prog);
    instruction * temp = NULL; //This is to hold a temp pointer when we realloc()
    int currentSize = 100;
    int curInstr = 4; //this is because of the intro Instructions
    parTagCount = 0; //Ensuring that the par tags are counted up from 0
    tokenTag res;
    instruction endInstr;
    endInstr.type = End;
    yyin = gcodeFile;
    res = yylex();
    while (res != END) {
        if (res == Instruction) {
//            printf("Instruction(%s)", yyval.strVal); // <-Used for debugging 
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
    if ((curInstr) < currentSize) { //the + 1 is for the End instruction 
        temp = realloc(prog, sizeof(instruction) * (curInstr + 1));
        if (temp != NULL) {
            prog = temp;
        }
        else {
            printf("Error in allocating memory when loading GCode into memory. Exiting\n");
            exit(1);
        }
    }
    prog[curInstr] = endInstr;
    //Add necessary symbols to symbol table
    GCode tempType = 0;
    currentSize = curInstr;
    for (curInstr = 0; curInstr < currentSize; curInstr++) {
        tempType = prog[curInstr].type;
        if (tempType == GLabel || tempType == CaseAlt) {
            insert(prog[curInstr].labelVal, &prog[curInstr]);
        }
        else if (tempType == FunDef) {
            insert(prog[curInstr].funVals.name, &prog[curInstr]);
        }
    }
    return prog;
}

