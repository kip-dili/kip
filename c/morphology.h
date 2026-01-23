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

// Apply handle helpers for amortizing apply_init across batches.
struct apply_handle* ups_handle_init(struct fsm* fsm);
struct apply_handle* downs_handle_init(struct fsm* fsm);
void apply_handle_free(struct apply_handle* h);

// Batch morphology helpers using a pre-initialized apply handle.
char ***ups_batch_handle(struct apply_handle* h, char **inputs, int count);
char ***downs_batch_handle(struct apply_handle* h, char **inputs, int count);

// Free the nested allocations returned by the batch functions.
void free_batch(char ***arr, int count);

#endif
