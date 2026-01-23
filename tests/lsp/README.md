# LSP Fixtures

This folder holds fixtures for `kip-lsp` tests. Each `.kip` file can have a
matching `.json` file with expectations.

## File layout

- `name.kip` — source fed to `kip-lsp`.
- `name.json` — expectations for that source (optional).

If the `.json` file is missing, all expectations default to false/empty.

## JSON schema

```json
{
  "diagnosticsAtLeast": 0,
  "diagnosticMessageContains": [],
  "formattingEdits": false,
  "formattingNoop": false,
  "hover": false,
  "hoverAt": { "line": 0, "character": 0 },
  "hoverContains": [],
  "completionIncludes": [],
  "completionAt": { "line": 0, "character": 0 },
  "cache": false,
  "cacheReuse": false,
  "definitionAt": { "line": 0, "character": 0, "atLeast": 1 }
}
```

Field meanings:

- `diagnosticsAtLeast`: minimum number of diagnostics expected after `didOpen`.
- `diagnosticMessageContains`: list of substrings that must appear in diagnostic messages.
- `formattingEdits`: whether `textDocument/formatting` should return edits.
- `formattingNoop`: whether `textDocument/formatting` should return no edits.
- `hover`: whether `textDocument/hover` should return a non-null result.
- `hoverAt`: position for the hover request (0-based line/character).
- `hoverContains`: substrings required to appear in hover contents.
- `completionIncludes`: list of labels that must appear in completion results.
- `completionAt`: position for the completion request (0-based line/character).
- `cache`: whether a `.iz` cache file should be written after `didSave`.
- `cacheReuse`: when true, run two sessions and ensure the cache timestamp does not change.
- `definitionAt`: request go-to-definition at the given position (0-based) and
  require at least `atLeast` locations; the returned URI must match the fixture.
