Tests are organized by expected outcome.

- `tests/succeed`: `.kip` files that must parse, typecheck, and evaluate.
- `tests/fail`: `.kip` files that must fail (parse/type/eval).
- Optional `.out`: exact expected output lines for a `.kip` in `tests/succeed`.
- Optional `.err`: substring that must appear in the error for a `.kip` in `tests/fail`.
- Optional `.in`: stdin to feed when running a `.kip` in `tests/succeed`.

Run with `stack test`. The test runner invokes the `kip` executable with
`--exec`, so make sure it is on your `PATH` (e.g. `stack install`) or set
`KIP_BIN` to its path.
