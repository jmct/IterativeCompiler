#include <string.h>
#include "util.h"

//String takes a pointer to a string and copies it to a newly allocated area of
//the heap. This allows functions that are passed 'string's as arguments to
//assume that the string they are passed will not be modified
string String(char *s) {
    string p = malloc(strlen(s) + 1);
    strcpy(p, s);
    return p;
}
