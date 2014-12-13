#include <stdlib.h>
#include "gthread.h"
#include "machine.h"

void initThreadPool(threadPool * pool) {
    pool->head = NULL;
    pool->tail = NULL;
    pool->numThreads = 0;
}

void addMachToThreadPool(Machine *mach, threadPool *pool) {
    threadQueueNode* newNode = malloc(sizeof(threadQueueNode));
    newNode->current = mach;
    newNode->next = NULL;
    if (pool->numThreads == 0) {
        pool->head = newNode;
    }
    else {
        pool->tail->next = newNode;
    }
    pool->tail = newNode;
    pool->numThreads += 1;
}

/*This is the version for non-profiled compilations */
void addQueueToThreadPool(threadQueueNode *lst, int numInLst, threadPool* pool) {
    threadQueueNode * lastInLst = lst;
    while (lastInLst->next != NULL) {
        lastInLst = lastInLst->next;
    }
    if (pool->numThreads == 0) {
        pool->head = lst;
        pool->tail = lastInLst;
    }
    else {
        pool->tail->next = lst;
        pool->tail = lastInLst;
    }
    pool->numThreads += numInLst;
    return;
}

void addQueueToThreadPoolProf(threadQueueNode *lst, int numInLst, threadPool* pool, unsigned int grc) {
    threadQueueNode * lastInLst = lst;
    while (lastInLst->next != NULL) {
        lastInLst = lastInLst->next;
    }
    if (pool->numThreads == 0) {
        pool->head = lst;
        pool->tail = lastInLst;
    }
    else {
        pool->tail->next = lst;
        pool->tail = lastInLst;
    }
    pool->numThreads += numInLst;

    /* This part reduces the efficiency of the function tremendously! 
     * TODO make profiling only
     * We need to iterate through the queue that's being added in order to
     * update the blockedCounter of each machine
     */
    int n;
    for (n = 0; n < numInLst; n++) {
        lst->current->blockedCounter += (grc - lst->current->blockTime);
        lst = lst->next;
    }
    return;
}

//TODO need to free queue node when it's no longer needed
Machine* getMachFromPool(threadPool* pool) {
    int numT = pool->numThreads;
    if (numT == 0) {
        return NULL;
    }
    threadQueueNode* tempForFreeing = NULL;
    Machine* headThread = pool->head->current;
    tempForFreeing = pool->head;
    pool->head = pool->head->next;
    free(tempForFreeing);
    if (numT == 1) {
        pool->head = NULL;
        pool->tail = NULL;
    }
    pool->numThreads -= 1;
    return headThread;
}
