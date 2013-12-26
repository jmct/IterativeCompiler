#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "instruction_type.h"
#include "symbolTable.h"

#define TABLE_SIZE 109

//this is the table itself. 
struct bucket* symbolTable[TABLE_SIZE];

//standard hashing
unsigned int hash(char* inputStr) {
    unsigned int result = 0;
    char * s;
    for (s = inputStr; *s; s++) { //when s dereferences to NULL the loop stops
        result = result * 65599 + *s;
    }
    
    return result;
}

//When all of the necessary information is available, we can make a new bucket
//The necessary 'next' value should be figured out by the calling function, 
//not this one. 
struct bucket* makeBucket(char* key1, instruction * binding1, struct bucket* next1) {
    struct bucket *b = malloc(sizeof(*b));
    b->key = key1;
    b->binding = binding1;
    b->next = next1;
    
    return b;
}

//given a key and value we can create a bucket and insert it
//into the symbol table
int insert(char* key, instruction * binding) {
    int index = hash(key) % TABLE_SIZE;
    symbolTable[index] = makeBucket(key, binding, symbolTable[index]);
    return index;
}


instruction * lookupKey(char* key) {
    int index = hash(key) % TABLE_SIZE;
    struct bucket* tmpBckt = symbolTable[0];
    for (tmpBckt = symbolTable[index]; tmpBckt; tmpBckt = tmpBckt->next) {
        if (strcmp(key, tmpBckt->key) == 0) {
            return tmpBckt->binding;
        }
    }
    //if we reach here, then there wasn't a match in the table
    return NULL;
}
