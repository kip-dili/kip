Tests are organized by expected outcome.

- `tests/succeed`: `.kip` files that must parse, typecheck, and evaluate.
- `tests/fail`: `.kip` files that must fail (parse/type/eval).
- Optional `.out`: exact expected output lines for a `.kip` in `tests/succeed`.
- Optional `.err`: substring that must appear in the error for a `.kip` in `tests/fail`.
- Optional `.in`: stdin to feed when running a `.kip` in `tests/succeed`.
- Optional `.args`: whitespace-separated CLI args to pass to `kip --exec`.
- Optional `.env`: per-test environment overrides (`KEY=VALUE`, one per line).

Run with `stack test`. The test runner invokes the `kip` executable with
`--exec`, so make sure it is on your `PATH` (e.g. `stack install`) or set
`KIP_BIN` to its path.

JS parity tests compare `kip --exec` output with `kip --codegen js` run under
Node.js (as ES modules to support top-level await). Ensure `node` is on your
`PATH` or set `NODE_BIN` to its path.

## Running Specific Test Subsets

You can run only specific tests using the `-p` pattern matcher:

```bash
# Run only JS tests
stack test --ta '-p "js"'

# Run only REPL tests
stack test --ta '-p "repl"'
```

The `-p` flag filters tests by their description, which includes the test file path.

## Reliable Performance Comparisons

Set `KIP_CACHE_DIR` to give each compiler build its own prelude snapshot when
measuring startup-heavy workloads. Warm each directory once before collecting
timings; alternating binaries through the default shared snapshot can otherwise
measure cache invalidation and reconstruction instead of compiler performance.

```bash
KIP_CACHE_DIR=/tmp/kip-before-cache /path/to/kip-before --exec program.kip
KIP_CACHE_DIR=/tmp/kip-after-cache /path/to/kip-after --exec program.kip

# Repeat the same commands after both snapshots have been created.
```

The override changes only the snapshot directory. Module caches next to source
files retain their existing behavior.
