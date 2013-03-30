struct HeapCell_ {
    int tag;
    char tester[10];
};

typedef struct HeapCell_ HeapCell;

struct chunk_ {
    struct chunk_ *previous;
    HeapCell **stack;
};

typedef struct chunk_ chunk;

struct stack_ {
    chunk * stackObj;
    HeapCell **stackPointer;
    HeapCell **framePointer;
};

typedef struct stack_ stack;

chunk * newChunk();

stack initStack(stack stk);

void stackOverflow(stack * stck);

void pushFrame(stack *stck);

void stackPush(HeapCell *addr, stack * stck);

void stackUnderflow(stack * stck);

void stackPopThrowAway(stack *stck);

HeapCell * stackPopKeep(stack * stck);

int itemsInFrame(stack * stck);

void popFrame(stack * stck);
