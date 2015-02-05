#include <stdlib.h>
#include <stdio.h>
#include "stack.h"

#define CHUNK_SIZE 200



chunk * newChunk() {
    HeapCell ** tempPtr = malloc(sizeof(HeapCell*) * CHUNK_SIZE);
    chunk * tempChunkPtr = malloc(sizeof(chunk));
    tempChunkPtr->stack = tempPtr;
    return tempChunkPtr;
};


stack initStack(stack stk) {
    stk.chunkSize = CHUNK_SIZE;
    stk.stackObj = newChunk();
    stk.stackObj->previous = NULL;
    stk.stackPointer = &stk.stackObj->stack[CHUNK_SIZE-1];
    stk.framePointer = NULL;
    *stk.stackPointer = NULL;
    return stk;
}

void freeStack(stack stck) {
    if (stck.stackObj->previous != NULL) {
        puts("Trying to free stack with multiple Chunks!");
    }
    free(stck.stackObj->stack);
    free(stck.stackObj);
}

void stackOverflow(stack * stck) {
    chunk *tempChunk = newChunk();
    tempChunk->previous = stck->stackObj;
    stck->stackObj = tempChunk;
    stck->stackPointer = &stck->stackObj->stack[CHUNK_SIZE-1];
}

/* Pushing a Frame
 *
 * When we push a frame we want to store two pieces of information:
 * The former frame pointer, and the as-of-yet computed return value.
 *
 * Step 1:
 * For the return value we copy the address of what's pointed to from 
 * the top of the stack currently and overwrite that stack slot with 
 * NULL (this becomes the slot where we will point to our return value). 
 *
 * Step 2a:
 * We also need to push the current program counter so that when we pop
 * we can return to the current instruction.
 *
 * Step 2b:
 * For the previous frame pointer we push the current frame pointer onto
 * the stack (casting it so that we don't get an error) and the update 
 * the frame pointer to point to that slot (which will currently be the
 * same as the stack pointer.
 * 
 * Step 3:
 * Push the address that we stored from the top of the last frame onto the
 * stack, this leaves us with the new frame as it should be
 */
void pushFrame(instruction * pc, stack *stck) {
    //step 1
    HeapCell * topOfOldStack = NULL;
    topOfOldStack = *stck->stackPointer;
    *stck->stackPointer = NULL;

    //step 2a
    stackPushEval((HeapCell*)pc, stck);

    //step 2b
    stackPushEval((HeapCell*)stck->framePointer, stck);
    stck->framePointer = stck->stackPointer;

    //step 3
    stackPush(topOfOldStack, stck);
//    printf("Pushing frame. return PC: %p\n", pc);
}

/*Pop stack frame:
 *
 * when popping a stack frame we want to revert the frame pointer to that
 * of the previous frame and ensure that whatever was pointed to at the
 * top of the current stack is still at the top when we complete the pop.
 *
 * Step 1:
 * We must be careful,
 * it is possible that there was a stack overflow between the return value
 * slot and the new frame pointer, so we have to check for that. Store the
 * value of where the return slot is into a temporary pointer. 
 *
 * Step 2:
 * Save the address of what the current stack pointer points to and assign
 * that value to the reserved spot (frame pointer + 1). 
 *
 * Step 3:
 * Set the stack pointer to point at the return value slot (frame pointer + 1).
 * 
 * Step 4:
 * set the Machine's progCounter to the value stored in the last frame
 * This is 1 item below the framePointer
 * 
 * Step 5:
 * Set the frame pointer to the value of what the previous frame pointer was.
 * This happens to be stored where the frame pointer points to.
 * 
 * Step 6:
 * Free all unneeded chunks
 */
instruction * popFrame(stack *stck) {
    instruction * newPC = NULL;
    chunk * curChunk = stck->stackObj;
    if (stck->framePointer == NULL) {
        printf("\tframePointer is Null\n");
        return NULL; //There's nothing to do!
    }
    //step 1
    HeapCell ** tempRetValPtr = NULL;
    tempRetValPtr = getNthAddrFrom(2, stck, stck->framePointer);

    //step 2
    *tempRetValPtr = *stck->stackPointer;

    //step 3
    stck->stackPointer = tempRetValPtr;

    //step 4
    newPC = (instruction*)*getNthAddrFrom(1, stck, stck->framePointer);
    
    //step 5
    stck->framePointer = (HeapCell**)(*stck->framePointer);

    //step 6
    chunk* oldChunk = NULL;
    while (!(stck->stackPointer >= curChunk->stack &&
             stck->stackPointer <= &curChunk->stack[CHUNK_SIZE-1])) {
        oldChunk = curChunk;
        curChunk = curChunk->previous;
        free(oldChunk->stack);
        free(oldChunk);
    }
//    printf("Popping frame. New PC: %p\n", newPC);
    stck->stackObj = curChunk;
    return newPC;
}

/*
void simulateFramePop(stack* stck, HeapPtr** framePtr, HeapPtr** stackPtr) {
    //when we actually pop the stack, we want the new stack pointer to point to
    //the returned (from the new frame) value, in this case we can skip that
    //therefore we look for the 3rd address from the framePointer instead of the
    //second
    HeapCell** tempNewStackPointer = getNthAddrFrom(3, stck, *framePtr);
    *stackPtr = tempNewStackPointer;

    //now all that is left to do is update the frame pointer, we don't have to
    //free any of the chinks since this isn't `really' a pop
    *framePtr = (HeapCell**)*(*framePtr);
}
 *
 * The way that push is implemented (as simply as possible) means that the
 * first stack element will start off empty and not be used. 
 * This is because the stack pointer is incremented before the assignment/
 */
void stackPush(HeapCell *addr, stack * stck) {
    if (isAddrInToSpace(addr, globalHeap) == 0)
        printf("Address is not in toSpace ");        
    if (stck->stackPointer == stck->stackObj->stack)
        stackOverflow(stck);
    else
        stck->stackPointer -= 1;
    *(stck->stackPointer) = addr;
}

//This is used so that the debugging code above does not fire when pushing
//during frame push
void stackPushEval(HeapCell *addr, stack * stck) {
    if (stck->stackPointer == stck->stackObj->stack)
        stackOverflow(stck);
    else
        stck->stackPointer -= 1;
    *(stck->stackPointer) = addr;
}

void stackUnderflow(stack * stck) {
    if (stck->stackObj->previous == NULL) {
        printf("Error: trying to pop more items than there are on the stack\n");
        exit(1);
    }
    chunk * tempChunkPtr = stck->stackObj;
    stck->stackObj = stck->stackObj->previous;
    stck->stackPointer = stck->stackObj->stack;
    free(tempChunkPtr->stack);
    free(tempChunkPtr);
}

void stackPopThrowAway(stack *stck) {
    if (stck->stackPointer == &stck->stackObj->stack[CHUNK_SIZE-1])
        stackUnderflow(stck);
    else
        stck->stackPointer += 1;
}

void popNFromStack(int n, stack* stck) {
    for (; n > 0; n--) {
        if (stck->stackPointer == &stck->stackObj->stack[CHUNK_SIZE-1]) {
            stackUnderflow(stck);
        }
        else {
            stck->stackPointer -= 1;
        }
    }
}

HeapCell * stackPopKeep(stack * stck) {
    HeapCell * tempHCPtr = NULL;
    tempHCPtr = *stck->stackPointer;
    stackPopThrowAway(stck);
    return tempHCPtr;
}

HeapCell * stackTopAddress(stack * stck) {
    return *stck->stackPointer;
}

int itemsInFrame(stack * stck) {
    if (stck->framePointer >= stck->stackObj->stack &&
        stck->framePointer <= &stck->stackObj->stack[CHUNK_SIZE-1]) {
        return stck->framePointer - stck->stackPointer;
    }
    else { // When the stack Pointer and the frame Pointer are not on the same chunk
        int res = (int)(&stck->stackObj->stack[CHUNK_SIZE-1] - stck->stackPointer) + 1;
        chunk* curChunk = stck->stackObj;
        if (stck->framePointer == NULL && curChunk->previous == NULL) {
            res = res - 1;
            return res;
        }
        else if (stck->framePointer == NULL) {
            curChunk = curChunk->previous;
            while (curChunk->previous != NULL) {
                res = res + CHUNK_SIZE;
                curChunk = curChunk->previous;
            }
            //The last slot in the last chunk does not store an item
            return res + (CHUNK_SIZE - 1); 
        }
        else {
            curChunk = curChunk->previous;
            while (!(stck->framePointer >= curChunk->stack &&
                     stck->framePointer <= &curChunk->stack[CHUNK_SIZE-1])) {
                res = res + CHUNK_SIZE;
                curChunk = curChunk->previous;
            }
            res = res + (int)(stck->framePointer - curChunk->stack);
            return res;
        }
    }
}

/*
int itemsInFakeFrame(stack* stck, HeapPtr* framePtr, HeapPtr* stackPtr) {
    //TODO we don't know if the stack pointer is on the same chunk as the fake
    //stack pointer... it may be easier to traverse stack and ensure that each
    //frame section is skipped
*/


HeapCell ** getNthAddrFrom(int n, stack* stck, HeapCell ** fromPtr) {
    chunk* curChunk = stck->stackObj;
    while (!(fromPtr >= curChunk->stack && fromPtr <= &curChunk->stack[CHUNK_SIZE-1])) {
        curChunk = curChunk->previous;
    }
    for (; n > 0; n--) {
        if (fromPtr == (&curChunk->stack[CHUNK_SIZE -1])) {
            curChunk = curChunk->previous;
            fromPtr = curChunk->stack;
        }
        else {
            fromPtr = fromPtr + 1;
        }
    }
    return fromPtr;
}

//This function is destructive, when it moves from one chunk to another, it
//frees the old chunk
HeapCell ** getNthAddrFromSP(int n, stack* stck) {
    chunk* oldChunk = NULL;
    HeapCell **fromPtr = stck->stackPointer;
    for (; n > 0; n--) {
        if (fromPtr == (&stck->stackObj->stack[CHUNK_SIZE -1])) {
            oldChunk = stck->stackObj;
            stck->stackObj = stck->stackObj->previous;
            fromPtr = stck->stackObj->stack;
            free(oldChunk->stack);
            free(oldChunk);
        }
        else {
            fromPtr = fromPtr + 1;
        }
    }
    return fromPtr;
}

HeapCell * getNthElement(int n, stack* stck) {
    HeapCell ** tempSP = stck->stackPointer;
    chunk * curChunk = stck->stackObj;
    int i;
    for (i = n; i > 0; i--) {
        if (tempSP == (&curChunk->stack[CHUNK_SIZE -1])) {
            curChunk = curChunk->previous;
            tempSP = curChunk->stack;
        }
        else {
            tempSP = tempSP + 1;
        }
    }
    return *tempSP;
}

int isPtrAtEndOfStack(stack* stck, HeapPtr* stackPtr) {
    chunk * curChunk = stck->stackObj;
    while (stackPtr < curChunk->stack || stackPtr > &curChunk->stack[CHUNK_SIZE -1]) {
        curChunk = curChunk->previous;
    }
    if (curChunk->previous == NULL &&
            (*stackPtr == NULL)) {
        return 1;
    }
    else {
        return 0;
    }
}
    
/*
void main() {
    stack myStack;
    myStack = initStack(myStack);
    HeapCell testCell;
    testCell.tag = 753;
    HeapCell testCell2;
    testCell2.tag = 75357;
    stackPush(&testCell, &myStack);
    pushFrame(&myStack);
    stackPush(&testCell, &myStack);
    pushFrame(&myStack);
    stackPush(&testCell, &myStack);
    stackPush(&testCell, &myStack);
    stackPush(&testCell, &myStack);
    int i;
    for (i = 0; i < 20; i++) {
        stackPush(&testCell2, &myStack);
    }
    printf("The numArgs should be low: %d\n", numArgs(&myStack));
} 
    //printf("Address of the heapCell: %p\nAddress pointed to at top of stack: %p\n\n", &testCell, *myStack.stackPointer);
    printf("Address of stackObject's stack in myStack's chunk: %p\n", myStack.stackObj->stack);
    printf("Address of stackPointer in myStack: %p\n", myStack.stackPointer);
    printf("Subtracting the former from the latter should be equal to chunk_size: %ld\n\n", 
            (myStack.stackPointer - myStack.stackObj->stack));
    printf("Now pushing 25 'items'...\n");
    int i = 0;
    for (i; i<45; i++) {
        printf("Push item %d\n", i+1);
        if (i == 38) {
            printf("testitem 2 is now pushed\n");
            stackPush(&testCell2, &myStack);
        }
        else {
            stackPush(&testCell, &myStack);
        }
    }
    printf("Address of stackObject's stack in myStack's chunk: %p\n", myStack.stackObj->stack);
    printf("Address of stackPointer in myStack: %p\n", myStack.stackPointer);
    printf("Subtracting the former from the latter should be equal to chunk_size - 6: %ld\n\n", 
            (myStack.stackPointer - myStack.stackObj->stack));
    printf("Now popping 5 items...(should not underflow)\n");
    for(i=0; i<5; i++) {
        stackPopThrowAway(&myStack);
    }
    printf("Now popping 1 item...(should underflow)\n");
    stackPopThrowAway(&myStack);
    printf("Address of the heapCell2: %p\nAddress pointed to at top of stack: %p\n\n", &testCell2, *myStack.stackPointer);
    printf("Now if I use stackPopKeep() I should get a pointer that points to the same thing: %p\n\n", stackPopKeep(&myStack));
*/
