#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include "instructions.h"

//A bucket contains the information for each symbol
//because we are using a symbol table with external chaining
//we need the buckets to form a linked list for when collisions
//occur
struct bucket {
    char * key;
    int binding;
    struct bucket * next;
};

unsigned int hash(char* inputStr);

struct bucket* makeBucket(char* key1, int binding1, struct bucket *next1);
int insert(char* key, int binding);
int lookupKey(char* key);

#endif
