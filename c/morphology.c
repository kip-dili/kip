#include "morphology.h"

#include<stdlib.h>
#include<stdbool.h>
#include<string.h>

#define LIMIT 50

// Morphological analysis (surface form -> analyses).
// Allocates a NULL-terminated array of strdup'd strings. Caller frees via
// `free` on the outer array and on each element.
char **ups(struct fsm* fsm, char *s)
{
  // Allocate array of LIMIT+1 pointers (extra for null terminator)
  // Previous code had bug: calloc(LIMIT, LIMIT * sizeof(char *)) allocated LIMIT*LIMIT*sizeof(char*)
  char **arr = (char **) calloc(LIMIT + 1, sizeof(char *));
  if (!arr) return NULL;

  struct apply_handle *h = apply_init(fsm);
  if (!h) { free(arr); return NULL; }

  char *result = apply_up(h, s);
  int i = 0;

  while (result != NULL && i < LIMIT)
  {
    arr[i++] = strdup(result);
    result = apply_up(h, NULL);  // Pass NULL for subsequent calls
  }
  // arr[i] is already NULL from calloc

  apply_clear(h);
  return arr;
}

// Morphological generation (analysis -> surface forms).
// Allocates a NULL-terminated array of strdup'd strings. Caller frees via
// `free` on the outer array and on each element.
char **downs(struct fsm* fsm, char *s)
{
  char **arr = (char **) calloc(LIMIT + 1, sizeof(char *));
  if (!arr) return NULL;

  struct apply_handle *h = apply_init(fsm);
  if (!h) { free(arr); return NULL; }

  char *result = apply_down(h, s);
  int i = 0;

  while (result != NULL && i < LIMIT)
  {
    arr[i++] = strdup(result);
    result = apply_down(h, NULL);  // Pass NULL for subsequent calls
  }
  // arr[i] is already NULL from calloc

  apply_clear(h);
  return arr;
}

// Batch morphological analysis (surface forms -> analyses).
// Reuses a single apply handle across inputs to amortize setup cost.
char ***ups_batch(struct fsm* fsm, char **inputs, int count)
{
  if (!inputs || count <= 0) return NULL;

  char ***out = (char ***) calloc(count, sizeof(char **));
  if (!out) return NULL;

  struct apply_handle *h = apply_init(fsm);
  if (!h) { free(out); return NULL; }

  for (int i = 0; i < count; i++)
  {
    char *s = inputs[i];
    if (!s) { out[i] = NULL; continue; }

    char **arr = (char **) calloc(LIMIT + 1, sizeof(char *));
    if (!arr) { out[i] = NULL; continue; }

    char *result = apply_up(h, s);
    int j = 0;
    while (result != NULL && j < LIMIT)
    {
      arr[j++] = strdup(result);
      result = apply_up(h, NULL);
    }
    out[i] = arr;
  }

  apply_clear(h);
  return out;
}

struct apply_handle* ups_handle_init(struct fsm* fsm)
{
  return apply_init(fsm);
}

struct apply_handle* downs_handle_init(struct fsm* fsm)
{
  return apply_init(fsm);
}

void apply_handle_free(struct apply_handle* h)
{
  if (!h) return;
  apply_clear(h);
}

// Batch morphological analysis (surface forms -> analyses) using a pre-initialized handle.
char ***ups_batch_handle(struct apply_handle* h, char **inputs, int count)
{
  if (!h || !inputs || count <= 0) return NULL;

  char ***out = (char ***) calloc(count, sizeof(char **));
  if (!out) return NULL;

  for (int i = 0; i < count; i++)
  {
    char *s = inputs[i];
    if (!s) { out[i] = NULL; continue; }

    char **arr = (char **) calloc(LIMIT + 1, sizeof(char *));
    if (!arr) { out[i] = NULL; continue; }

    char *result = apply_up(h, s);
    int j = 0;
    while (result != NULL && j < LIMIT)
    {
      arr[j++] = strdup(result);
      result = apply_up(h, NULL);
    }
    out[i] = arr;
  }

  return out;
}

// Batch morphological generation (analysis -> surface forms).
// Reuses a single apply handle across inputs to amortize setup cost.
char ***downs_batch(struct fsm* fsm, char **inputs, int count)
{
  if (!inputs || count <= 0) return NULL;

  char ***out = (char ***) calloc(count, sizeof(char **));
  if (!out) return NULL;

  struct apply_handle *h = apply_init(fsm);
  if (!h) { free(out); return NULL; }

  for (int i = 0; i < count; i++)
  {
    char *s = inputs[i];
    if (!s) { out[i] = NULL; continue; }

    char **arr = (char **) calloc(LIMIT + 1, sizeof(char *));
    if (!arr) { out[i] = NULL; continue; }

    char *result = apply_down(h, s);
    int j = 0;
    while (result != NULL && j < LIMIT)
    {
      arr[j++] = strdup(result);
      result = apply_down(h, NULL);
    }
    out[i] = arr;
  }

  apply_clear(h);
  return out;
}

// Batch morphological generation (analysis -> surface forms) using a pre-initialized handle.
char ***downs_batch_handle(struct apply_handle* h, char **inputs, int count)
{
  if (!h || !inputs || count <= 0) return NULL;

  char ***out = (char ***) calloc(count, sizeof(char **));
  if (!out) return NULL;

  for (int i = 0; i < count; i++)
  {
    char *s = inputs[i];
    if (!s) { out[i] = NULL; continue; }

    char **arr = (char **) calloc(LIMIT + 1, sizeof(char *));
    if (!arr) { out[i] = NULL; continue; }

    char *result = apply_down(h, s);
    int j = 0;
    while (result != NULL && j < LIMIT)
    {
      arr[j++] = strdup(result);
      result = apply_down(h, NULL);
    }
    out[i] = arr;
  }

  return out;
}

// Free the nested allocations produced by `ups_batch`/`downs_batch`.
void free_batch(char ***arr, int count)
{
  if (!arr) return;
  for (int i = 0; i < count; i++)
  {
    char **row = arr[i];
    if (!row) continue;
    for (int j = 0; row[j] != NULL; j++)
    {
      free(row[j]);
    }
    free(row);
  }
  free(arr);
}
