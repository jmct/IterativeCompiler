#ifndef GINSTRUCTION_H
#define GINSTRUCTION_H

#include <assert.h>
#include "instruction_type.h"
#include "symbolTable.h"
#include "heap.h"
#include "stack.h"
#include "gthread.h"
#include "machine.h"

extern unsigned int evalPrintLoop;

enum ExecutionMode_ {
    LIVE,   
    BLOCKED,
    FINISHED,
    UNKNOWN
};

typedef enum ExecutionMode_ ExecutionMode;

void slideNStack(int n, Machine *mach);
void pushGlobal(instruction *fun, Machine *mach);
void pushInt(int val, Machine * mach);
void mkAp(Machine *mach);
void push(int offset, Machine *mach);
void slide(int num, Machine *mach);
void pop(int num, Machine *mach);
void unlock(HeapPtr node);
void update(int num, Machine *mach);
void alloc(int num, Machine *mach);
int numArgs(Machine *mach);
void rearrangeStack(int num, stack *stck);
ExecutionMode unwind(Machine* mach);
void eval(Machine *mach);
void addI(Machine *mach);
void subI(Machine *mach);
void mulI(Machine *mach);
void divI(Machine *mach);
void negI(Machine *mach);
void eqI(Machine *mach);
void neI(Machine *mach);
void ltI(Machine *mach);
void leI(Machine *mach);
void gtI(Machine *mach);
void geI(Machine *mach);
void casejump(char *label, Machine *mach);
void caseAltEnd(char *label, Machine *mach);
void split(int num, Machine * mach);
void pack(int tag, int ar, Machine *mach);
void printI(Machine *mach);
void parI(Machine* mach, threadPool* pool);


#endif
