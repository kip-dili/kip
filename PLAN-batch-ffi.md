# Batch FFI Plan (ups/downs)

## Goal
Amortize Foma `apply_init` cost by batching multiple `ups`/`downs` calls through a single handle per batch.

## Scope
- C layer: add batch entry points that accept arrays of input strings and return arrays of result arrays.
- Haskell layer: add `upsBatch`/`downsBatch` functions, and thread usage into parser/render paths that currently call `ups`/`downs` in loops.
- Keep single-call APIs for simplicity and fallback.

## Design
1. C API additions in `c/morphology.c`:
   - `ups_batch(struct fsm* fsm, char **inputs, int count)` returning `char***` (array of `char**`), each `char**` is the current per-input result array as returned by `ups`.
   - `downs_batch(struct fsm* fsm, char **inputs, int count)` similarly.
   - Implement with a single `apply_handle` and reuse `apply_up`/`apply_down` for each input, resetting the handle between inputs with `apply_clear` + `apply_init` or `apply_reset` if available.
   - Provide a `free_batch(char*** arr, int count)` helper to free all nested allocations.

2. Haskell FFI in `src/Language/Foma.hs`:
   - Add FFI imports for `ups_batch`, `downs_batch`, and `free_batch`.
   - Implement `upsBatch :: FSM -> [Text] -> IO [[Text]]` and `downsBatch :: FSM -> [Text] -> IO [[Text]]`.
   - Marshal `[Text]` to `[CString]`, call batch function, read `char***`, convert to `[[Text]]`, free via `free_batch`.

3. Integration points:
   - Parser: batch calls for `downsCached` in loops where multiple stems are generated at once (e.g., `matchCtxByInflection`, candidate estimation paths).
   - Render: batch `downsCached`/`upsCached` where we map over multiple stems or analyses.

4. Safety & correctness:
   - Maintain current LIMIT cap per input.
   - Ensure all C allocations are freed.
   - Preserve existing caches; batched calls should still cache per input string.

5. Validation
   - Build (`stack build`) and run tests (`stack test`).
   - Quick profiling run to confirm reduced time in `Language.Foma.downs/ups` cost centers.
