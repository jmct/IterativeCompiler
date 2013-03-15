//These are the declarations of both the GCode type and the instruction type.
#ifndef INSTRUCTION_H
#define INSTRUCTION_H

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
         CaseAlt,
         CaseAltEnd,
         Split, 
         Print,
         Par
} GCode;

typedef int codePtr;

struct _instruction {
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
typedef struct _instruction instruction;

#endif
