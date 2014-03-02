#ifndef SEARCH_HEADER
#define SEARCH_HEADER
#include "machine.h"
#include "stats.h"
#include "instruction_type.h"

enum searchTypes_ {
    RAND,
    NONE
};

parSwitch* iterate(parSwitch* switches, int numSwitch, StatTable* gStat, enum searchTypes_ sType, int iFlag, instruction* prog);

#endif
