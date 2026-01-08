#ifndef KIP_MORPHOLOGY_H
#define KIP_MORPHOLOGY_H

#include <fomalib.h>

// Single-input morphology helpers.
char **ups(struct fsm* fsm, char *s);
char **downs(struct fsm* fsm, char *s);

// Batch morphology helpers. The result is an array of `char**` entries,
// one per input, each terminated with a NULL pointer.
char ***ups_batch(struct fsm* fsm, char **inputs, int count);
char ***downs_batch(struct fsm* fsm, char **inputs, int count);

// Free the nested allocations returned by the batch functions.
void free_batch(char ***arr, int count);

#endif
