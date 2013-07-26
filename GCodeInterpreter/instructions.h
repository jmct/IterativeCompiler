//These are the declarations of both the GCode type and the instruction type.
#ifndef INSTRUCTION_H
#define INSTRUCTION_H
#include <stdio.h>

/* This typedef is just simple syntactic sugar for bools */
typedef enum {
    FALSE,
    TRUE
} Bool;

typedef enum {       //Number of Arguments
         End,
         Unwind,     //0 
         PushGlobal, //1 
         PushInt,    //1
         Push,       //1
         MkAp,       //0
         Update,     //1
         Pop,        //1
         Slide,      //1
         Alloc,      //1
         Eval,       //0
         Add, Sub, Mul, Div, Neg, //0
         Eq, Ne, Lt, Le, Gt, Ge,  //0
         Pack,       //2
         CaseJump,   //1
         CaseAlt,    //1
         CaseAltEnd, //1
         Split,      //1
         GLabel,      //1
         FunDef,      //2
         Print,      //0
         Par         //0
} GCode;

typedef int codePtr;

struct _instruction {
    GCode type;
    union {
        char * pushGlobVal;
        char * labelVal;
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
            int tag, arity;
        } packVals;
    };
};
typedef struct _instruction instruction;


typedef struct {
    Bool pswitch;
    int address;
} parSwitch;

//Function to iterate over and parse .gcode file. Returns a pointer to an array
//of instructions (the program).
instruction *parseGCode(FILE* gcodeFile, parSwitch** parSwitchesPtr);

//Take returned value from a yylex() call and create an instruction type
instruction makeInstruction(char *instr);

#endif
