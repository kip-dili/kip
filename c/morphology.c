#include "morphology.h"

#include<stdlib.h>
#include<stdbool.h>
#include<string.h>

#define LIMIT 50


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

// Free the nested allocations produced by the batch handle functions.
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
