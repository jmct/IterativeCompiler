#include <stdlib.h>
#include "gthread.h"
#include "machine.h"

void addMachToThreadPool(Machine* mach, threadPool* pool) {
    threadQueueNode* newNode = malloc(sizeof(threadQueueNode));
    newNode->current = mach;
    newNode->next = NULL;
    if (pool->numThreads == 0) {
        pool->head = newNode;
    }
    pool->tail->next = newNode;
    pool->tail = newNode;
    pool->numThreads += 1;
}

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

//TODO need to free queue node when it's no longer needed
Machine* getMachFromPool(threadPool* pool) {
    int numT = pool->numThreads;
    if (numT == 0) {
        return NULL;
    }
    Machine* headThread = pool->head->current;
    pool->head = pool->head->next;
    if (numT == 1) {
        pool->head = NULL;
        pool->tail = NULL;
    }
    pool->numThreads -= 1;
    return headThread;
}
