#include <stdlib.h>
#include <string.h>

//A bucket contains the information for each symbol
//because we are using a symbol table with external chaining
//we need the buckets to form a linked list for when collisions
//occur
struct bucket {
    char * key;
    int binding;
    struct bucket * next;
};

#define TABLE_SIZE 109

//this is the table itself. 
struct bucket* symbolTable[TABLE_SIZE];

//standard hashing
unsigned int hash(char* inputStr) {
    unsigned int result;
    char * s;
    for (s = inputStr; *s; s++) { //when s dereferences to NULL the loop stops
        result = result * 65599 + *s;
    }
    
    return result;
}

//When all of the necessary information is available, we can make a new bucket
//The necessary 'next' value should be figured out by the calling function, 
//not this one. 
struct bucket* makeBucket(char* key1, void* binding1, struct bucket* next1) {
    struct bucket* b = malloc(sizeof(*b));
    b->key = key1;
    b->binding = binding1;
    b->next = next1;
    
    return b;
}

//given a key and value we can create a bucket and insert it
//into the symbol table
void insert(char* key, void* binding) {
    int index = hash(key);
    symbolTable[index] = makeBucket(key, binding, symbolTable[index]);
}


int lookupKey(char* key) {
    int index = hash(key);
    struct bucket* tmpBckt = NULL;
    for (tmpBckt = symbolTable[index]; tmpBckt; tmpBckt = tmpBckt->next) {
        if (strcmp(key, tmpBckt->key) == 0) {
            return tmpBckt->binding;
        }
    }
    //if we reach here, then there wasn't a match in the table
    return NULL;
}












