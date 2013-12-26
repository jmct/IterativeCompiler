#ifndef STACK_H
#define STACK_H
#include "heap.h"
#include "instruction_type.h"

struct chunk_ {
    struct chunk_ *previous;
    HeapCell **stack;
};

typedef struct chunk_ chunk;

struct stack_ {
    int chunkSize;
    chunk * stackObj;
    HeapCell **stackPointer;
    HeapCell **framePointer;
};

typedef struct stack_ stack;

chunk * newChunk();

stack initStack(stack stk);

void freeStack(stack stck);

void stackOverflow(stack * stck);

void pushFrame(instruction *pc, stack *stck);

void stackPush(HeapCell *addr, stack * stck);
void stackPushEval(HeapCell *addr, stack * stck);

void stackUnderflow(stack * stck);

void stackPopThrowAway(stack *stck);

void popNFromStack(int n, stack* stck);

HeapCell * stackPopKeep(stack * stck);

int itemsInFrame(stack * stck);

instruction *popFrame(stack *stck);

HeapCell * getNthElement(int n, stack * stck);

HeapCell ** getNthAddrFrom(int n, stack* stck, HeapCell ** fromPtr);

HeapCell ** getNthAddrFromSP(int n, stack* stck);

//void simulateFramePop(stack* stck, HeapPtr** framePtr, HeapPtr**stackPtr);

int isPtrAtEndOfStack(stack* stck, HeapPtr* stackPtr);

//int itemsInFakeFrame(stack* stck, HeapPtr* frame, HeapPtr* stackPtr);

#endif
