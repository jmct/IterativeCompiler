#include <stdio.h>
#include <stdlib.h>
#include "heap.c"

#define HEAPSIZE 10000
#define STACK_SIZE 3000
#define FRAME_STACK_SIZE 100


typedef struct {
    HeapCell * stack[STACK_SIZE];
    int stackPointer;
    int frameStack[FRAME_STACK_SIZE];
} Machine;

//Pushing an arbitrary item onto the stack
void pushStack(HeapCell* item, Machine* mach) {
    mach->stack[mach->stackPointer + 1] = item;
    mach->stackPointer++;
}

typedef struct {
    HeapCell * current;
    HeapCell * retPointer;
} Frame;



int main() {
    Machine machineA;
    printf("machineA's stack pointer is at: %d\n", machineA.stackPointer);
    myHeap = malloc(HEAPSIZE * sizeof(HeapCell));
    printf("Free: %d, Pointer Value %p\n", nextFree, myHeap);
    Heap point = allocHeapCell(APP, myHeap); 
    printf("Free: %d, Pointer Value %p\n", nextFree, point);
    pushStack(point, &machineA);
    printf("machineA's stack pointer is at: %d\n", machineA.stackPointer);
    point = allocHeapCell(APP, myHeap); 
    printf("Free: %d, Pointer Value %p\n", nextFree, point);

    return 0;
}
