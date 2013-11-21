#ifndef GTHREADS_HEADER
#define GTHREADS_HEADER

//thread queues will be used both for blocked lists and the spark pool
struct threadQueueNode_ {
    struct Machine_* current;
    struct threadQueueNode_* next;
};

typedef struct threadQueueNode_ threadQueueNode;

struct threadPool_ {
    threadQueueNode* head;
    threadQueueNode* tail;
    int numThreads;
};

typedef struct threadPool_ threadPool;

void initThreadPool(threadPool * pool);
void addMachToThreadPool(struct Machine_* mach, threadPool* pool);
void addQueueToThreadPool(threadQueueNode *lst, int numInLst, threadPool* pool);
void addQueueToThreadPoolProf(threadQueueNode *lst, int numInLst, threadPool* pool, unsigned int grc);
struct Machine_* getMachFromPool(threadPool* pool);
#endif
